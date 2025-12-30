using DataFrames

using Distributed
using StatsBase

 # Set up multiprocessing.
try
    num_cores = parse(Int, ENV["SLURM_CPUS_PER_TASK"])
    addprocs(num_cores)
catch
    desired_nprocs = length(Sys.cpu_info())

    if length(procs()) != desired_nprocs
        addprocs(desired_nprocs - 1)
    end
end
 

@everywhere using DrWatson
@everywhere quickactivate("..")
@everywhere include("../code/models/model_bidirectional_twogroup_influence.jl") 

function social_influence_experiment(nagents = 1000; #number of :RegularAgents
                                    nsteps = 200, 
                                    z = 0.02, #increment of influence 
                                    nreplicates = 100, #n repeated simulations
                                    nteachers = 1, #number of teachers agents learn from at each time step
                                    aversion = collect(0.1:0.2:0.9), #aversion = [collect(0.1:0.1:0.9)..., 0.99], #outgroup aversion -- everybody has the same level of aversion right now
                                    ninfluencers = 20, #number of agents to specify as :Influencers: let's break it down to 10 for group 1, 10 for group 2 OR zero (baseline for comparison)
                                    n_early_adopters = 10, # number of early protective behavior adoptors in group 1
                                    g1_influencer_message = collect(0:1:1), #group 1 influencer message: positive (1) or negative (0)
                                    g2_influencer_message = collect(0:1:1), #group 2 influencer message: positive (1) or negative (0)
                                    # below: stand-ins for group-level homophily, used as proportions for the random agent sample draws
                                    w1 = collect(0.1:0.2:0.9), #w1 = collect([0.5,0.7,0.9,0.99]),  #group 1 homophily
                                    w2 = collect(0.1:0.2:0.9), #w2 = collect([0.5,0.7,0.9,0.99]), #group 2 homophily
                                    f = collect([3,5]), #f = collect([3,5,99]), #number of other agents in encounter_pool (for observing-to-learning threshold): 
                                                           #fixed: 3 vs 5; where f = 99, model transforms to a random Int from a Poisson distribution with λ = 3
                                    f_status = "fixed", #remains "fixed" if f = 3 or 5, and changes to "random" if f = 99 (this value causes a random draw under the hood; see: `f` above)
                                    allsteps = false
    ) 

    #Fixed Parameters
    b_protect = 0.85 #the effectiveness of the protective behavior (0.0-1.0)
    #0.85 after Leung et al 2020 https://doi-org.stanford.idm.oclc.org/10.1038/s41591-020-0843-2 
    δt = 1.0 #length of time step: must be a float/double
#=     f = 3 #used in encounter_baseline -- rand(Poisson(f)) -- well-mixed approach to choosing number of encounters 
         #agents need to make a decision re: behavioral adoption. Learning step equivalent to using c parameter. =#
    tf = nsteps*δt #the length of the time domain
    t = 0:δt:tf #time series vector: construct a range from 0 to tf with stepsize δt


    ## Parameter values ##
    τ = 0.02 #transmissibility
    c = 3.0*δt #contact rate from which we get λ for the random draw from a Poisson distribution for each 
                #transmission instance (infection and learning)
    β = τ*c

    ## Utility function to convert a rate to a proportion (used in Parameters) ##
    function rate_to_proportion(r::Float64,t::Float64)
        1-exp(-r*t)
    end
    γ = rate_to_proportion(0.05,δt) #used in recovery -- consider what this recovery rate implies proportionately

    #helper R0
    R0 = β/γ

    #if interested in setting your own R0, you can derive it like this:
    #β = R0*γ

    N = nagents+ninfluencers #number of agents
    I0 = 10 #number of initial infections

    rep_idx = collect(1:nreplicates)

    #parameters that will change for each model respecification
    params_list = dict_list(
        @dict aversion g1_influencer_message g2_influencer_message w1 w2 f rep_idx ninfluencers f_status
        )

    
    models = [init_model(N;β,c,γ,I0,b_protect,n_early_adopters,nteachers,z,params...) 
              for params in params_list]

   function stopfn_fixated(model, step)
        agents = allagents(model) 
        #stop running when there are no more infected agents
        return (
            all(agent.status != :I for agent in agents)
        )
    end 

    adata = [:group,:status,:behavior,:infvalue,:infstatus,:observelearn,:infalterid,:infkeep,:newly_infected]
    mdata = [:w1,:w2,:aversion,:g1_influencer_message,:g2_influencer_message,:rep_idx,:f,:ninfluencers,:f_status]

    # For now ignore non-extremal time steps.
  
    #when(model, step) = stopfn_fixated(model, step)
    adf, mdf = ensemblerun!(models, agent_step!, dummystep, stopfn_fixated; #stopfn_fixated;
                            adata, mdata, parallel = true,  # when -- this was causing to collect at the end: see: https://juliadynamics.github.io/Agents.jl/dev/tutorial/
                            showprogress = true)

    ### process raw results ###

    #number of agents by :status (:S, :I, or :R), group, step and ensemble
    abm_data_status = combine(groupby(adf, [:step,:ensemble,:group,:status]), nrow => :n_status) 

    #number of agents by :behavior (1 or 0 -- i.e., did it or didn't do it), group, step and ensemble
    abm_data_behavior = combine(groupby(adf, [:step,:ensemble,:group,:behavior]), nrow => :n_behavior) 
    
    #number of newly infected agents at each time step by group for infection CDF
    #will need to calculate the cumulative after this but this gets us the base value
    temp = filter(row -> row.newly_infected == 1, adf)
    status_cdf = combine(groupby(temp, [:step,:ensemble,:group]), nrow => :n_new_infections) 

    #collapse counts and infection CDF data into one df
    results = innerjoin(abm_data_status, abm_data_behavior, on = [:step,:ensemble,:group])
    results = innerjoin(results, status_cdf, on = [:step,:ensemble,:group])

    results = innerjoin(results, mdf, on = [:step, :ensemble])


    #get influencer-specific information
    temp = innerjoin(adf,mdf,on = [:step, :ensemble])

    #how many individuals per step and group are being influenced?
    temp = filter(row -> row.infvalue > 0 && row.infstatus == :RegularAgent, temp)

    temp = combine(groupby(temp, [:step,:ensemble,:group]), nrow => :ninfluenced_agents_bystep)

    results = innerjoin(results,temp, on = [:step, :ensemble,:group])

    #elasticity of influence by influencer value: i.e., extra number of agentsworth of influence
    #an influencer is exerting at each step
    temp = innerjoin(adf,mdf,on = [:step, :ensemble])

    #how many individuals per step and group are being influenced?
    temp = filter(row -> row.infvalue > 0, temp) #keep
    temp1 = combine(groupby(temp, [:ensemble,:step,:group]), :infvalue => sum)
    rename!(temp1,:infvalue_sum => :infvalue_bygroupsum)

    results = innerjoin(results,temp1, on = [:ensemble,:step,:group])

    temp2 = combine(groupby(temp, [:ensemble,:step,:infalterid]), :infvalue => sum)
    rename!(temp2,:infvalue_sum => :infvalue_byaltersum)

    results = innerjoin(results,temp2, on = [:step, :ensemble])
 
    #a list of all regular agents each influencer interacted with
    infalterid_socialnetwork = combine(groupby(adf, [:ensemble,:step,:infalterid]),[:id] .=> Ref, renamecols=false)

    results = innerjoin(results,infalterid_socialnetwork, on = [:step, :ensemble,:infalterid])

    return results
end





