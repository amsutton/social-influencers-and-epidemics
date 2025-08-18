
# Agent-based model of social influence in the context of coupled contagion

#INCLUDES COUNTER FOR ENCOUNTERS, AND OUTGROUP AVERSION
#ALSO INCLUDES TWO KINDS OF INFLUENCERS (both positive and negative) -- each group can 
#be influenced separately

#Stanford University -- Winter 2024

######################################################################################
######################################################################################
#Authors: Aja Sutton, Postdoctoral Scholar at Stanford Doerr School of Sustainability, 
#Matthew A. Turner, (fellowship info)
#James Holland Jones, Professor of Environmental Social Behavior 


#In this version, agents learn to adopt a protective behavior from each other,
#and can infect each other. They also are able to have their social learning affected
#by modeled behavior on the part of "social influencer" agents, who cannot learn from
#others and do not participate in the epidemiological stage 
#(i.e. approximating online media influence that does not occur in-person 
#and is typically not a strong discourse between influencers and the influenced)

# Informed by Simon Frost (@sdwfrost)'s epirecipes: SIR for Julia using Agents.jl, 2020-04-27
# accessible at: https://github.com/epirecipes/sir-julia/blob/master/markdown/abm/abm.md


# based in part on work by:
#Smaldino, P., & Jones, J. (2021). Coupled dynamics of behaviour and disease 
#contagion among antagonistic groups. Evolutionary Human Sciences, 3, E28. 
#doi:10.1017/ehs.2021.22
######################################################################################
######################################################################################

#Julia packages
using Agents
using DrWatson: @dict
using StatsBase
using Distributions: Poisson

### Functions ###

## Define agent struct ##

mutable struct Person <: AbstractAgent
    #We define each agent:
    id::Int #individually (`id`)
    group::Int #by `group` (i.e. 1 or 2)
    w::Float64 #with a group-level weight variable that represents homophily, `w`: determines probability of sampling in-group members
    behavior::Int #with a behavioral learning status `behavior`
    pbehavior::Int #previous behavior `pbehavior` as helper
    status::Symbol #with a (health) `status` (`:S`,`:I`, or `:R`, in an SIR model)
    pstatus::Symbol #previous status `pstatus` as helper
    #learntest::Int64 #tracking when and how learning occurs
    infstatus::Symbol #whether the agent is an influencer or not. Values: :Influencer and :RegularAgent
    infvalue::Float64 #ability to influence (increases incrementally by `z`, currently a global parameter)
    #unused #infseed::Float64 #increment of `z` -- could be useful in the future if influence must vary, for now a global value is sufficient: `z`
    infkeep::Symbol #whether a regular agent keeps the influencer :K keep vs :D discard
    infalterid::Int #ability for the agent to remember the specific influencer
    encounter_pool::Array #A dictionary of pairs representing previously encountered alters for behavioral learning re: decision/adoption, 
                          #where `k` = "ingroup" or "outgroup", and `v` = 1 (has adopted behavior) and 0 (has not adopted behavior)
    encounter_baseline::Int #the number of encounters the agent must make before deciding whether to adopt the behavior
    observelearn::Symbol #has the agent decided to make a decision to learn or not? 
                         # Based on meeting threshold number of alters: `encounter_baseline`. Values: :observing and :learning
    newly_infected::Int #default is 0, becomes 1 when the agent is freshly infected (a helper value for calculating CDF of infections) -- this is reset to zero the next time the agent stepped in agent.step!
end


function init_model(N::Int; β::Float64, c::Float64, γ::Float64, I0::Int, b_protect::Float64, f::Int, f_status="fixed", z::Float64,
                    w1::Float64, w2::Float64, aversion::Float64, g1_influencer_message::Int, g2_influencer_message::Int,
                    n_early_adopters::Int, ninfluencers::Int, nteachers::Int, rep_idx=nothing, model_parameters...)
    #set parameter fields, and add agents to the model, some of whom are infected:
    #β:  probability of infection, 
    #γ: probability of recovery per capita, 
    #b_protect: effectiveness of the protective behavior against infection (0.0-1.0)
    #c: constant; determines number of contacts susceptible individual makes per time step (the sample), and 
    #w: is proportion of the sample of agents within one's own group vs out-gropu (group weight): i.e. homophily

    #if f, the working memory of an agent for the n other agents in the encounter_pool, has the dummy Int value 99, 
    #actually just draw a random Int from a Poisson distribution where λ = 3
    if f == 99
        f = rand(Poisson(3))
        f_status = "random"
    end


    properties = @dict N β c γ b_protect ninfluencers n_early_adopters aversion rep_idx w1 w2 g1_influencer_message g2_influencer_message nteachers z f f_status
    model = ABM(Person; properties=properties)
    
    #early adopters begin in group 1, where you would expect them because this is where infection also begins
    earlyadopters = rand(1:round(Int,((N-ninfluencers)/2)), n_early_adopters)

    for i in 1:1:N

        newly_infected = 0
        id = i

        if i ≤ round(Int,((N-ninfluencers)/2))
            group = 1 
            w = w1 #homophily of group 1
        elseif i ≥ round(Int,((N-ninfluencers)/2))
            group = 2 #group 2
            w = w2 #homphily of group 2
        end

        # add early adopters of the behavior, randomly, within group 1
        #default behaviors
        behavior = 0
        pbehavior = 0

        if i in earlyadopters
            behavior = 1 #behavior
            pbehavior = 1 #past behavior (initialized here as helper to transition out of step 0)
        end

        if i ≤ I0 #if person is less than equal to Index, i.e. initial infection cases
            status = :I
            pstatus = :I #transition out of step zero helper
        else #otherwise susceptible
            status = :S
            pstatus = :S #transition out of step zero helper
        end

        #social influencer status and values
        infstatus = :RegularAgent
        infvalue = 0
        infkeep = :null
        infalterid = 0
        
        encounter_pool = [] #placeholder to assure this works later
        #encounter_baseline = 0
        #while encounter_baseline == 0
        encounter_baseline =  f #rand(Poisson(f))
        #end
        
        observelearn = :observing

        #if there are i's above N = n :RegularAgents,
        #then there are :Influencers
        if i > N-ninfluencers
            infstatus = :Influencer
            infkeep = :K

            #ten influencers for both group 1 and group 2
            #Julia does not have consistent indexing (it randomly orders all entries in a Dictionary)
            #so order doesn't matter, just quantity here
                            
            if ninfluencers == 20 
                if i > N-ninfluencers && i ≤ N-(ninfluencers/2)
                    group = 1
                else
                    group = 2
                end
     #=        elseif ninfluencers == 10
                if g1_influencer_message == 1 && g2_influencer_message == 0
                    g = 1
                end
                if g1_influencer_message == 0 && g2_influencer_message == 1
                    g = 2
                end =#
            end

        end

        if infstatus == :Influencer
            status = :null
            pstatus = :null

            if group == 1
                behavior = g1_influencer_message
                pbehavior = g1_influencer_message
            else
                behavior = g2_influencer_message
                pbehavior = g2_influencer_message
            end

        end 

        agent_to_add = Person(id, group, w, behavior, pbehavior, status, pstatus, infstatus, infvalue, infkeep, infalterid, encounter_pool, encounter_baseline, observelearn, newly_infected) # add them to the model
        agents = add_agent!(agent_to_add,model) #put it all together
    end

    return model

end



## Functions to implement in the model ##
function sample_agents!(agent, model)
    #A function to select the agent's sample for the current time step with group homophily
    #For either social learning or disease transmission:
    #returns a dictionary of alter agents: `alter`

    #set number of contacts: as a function of c, the constant sample number of contacts 
    #(ncontacts) from group one or two with group homophily weights
    #this is actually the λ of the Poisson distribution: it sets the shape of the distribution
    ncontacts = rand(Poisson(model.properties[:c]))

    #let's assume that one "sticky" influencer per agent is enough for now. 
    #also, let's assume agents can only have influencers from their own group.
    #This makes sense because of the premise of in-group homophily.

                #COMPLETE ASIDE: One example of behavior changing and stickiness that isn't used here, 
                #(we focus on the stickiness of the influencer rather than the strategy) but could be in the future is a Fermi function:
                #Pr(strategy_i switches to strategy_j) = 1/1+exp(Δ_ϵij/κ), where Δ_ϵij = is the difference between 
                #the payoff of strategies i or j for individuals, and κ is the 'stickiness' of the behavior. 
                #See: equation 3.2 in Verelst et al 2016, and originally:
                #Fukuda E, Tanimoto J, Akimoto M. 2015 Influence of breaking the symmetry between disease transmission 
                #and information propagation networks on stepwise decisions concerning vaccination. Chaos Solitons Fractals 80, 
                #47– 55. (doi:10.1016/j.chaos. 2015.04.018)
   
    #if the agent doesn't have a sticky influencer yet, they get to sample from all groups, 
    #and might randomly meet an influencer that will become sticky during the learning stage.
    if agent.infalterid == 0

        if agent.group == 1
            ncontacts = sample([1,2], weights([(agent.w), (1-agent.w)]), ncontacts)
        elseif agent.group == 2
            ncontacts = sample([2,1], weights([(agent.w), (1-agent.w)]), ncontacts)
        end

        #gather ncontacts, so we know how many are in group 1 and group 2
        counts1 = filter(((v),) ->  v == 1, ncontacts)
        counts1 = length(counts1)
        counts2 = filter(((v),) ->  v == 2, ncontacts)
        counts2 = length(counts2) 

        #filter all the agents in the model such that it excludes the current acting agent 
        #(i.e. agent cannot meet and infect itself) -- unlikely but let's be explicit
        altergroup = filter(((k,v),) ->  k != agent.id, model.agents) 

        #filter altergroup into groups 1 and 2, and then randomly sample individuals 
        #from those groups based on the counts in the altergroup

        if agent.group == 1
            g1 = filter(((k,v),) ->  v.group == 1, altergroup) #could be influencers or regular agents
            temp1 = sample(collect(keys(g1)), counts1, replace = false)
            g1 = filter(((k,v),) -> k in temp1, g1)

            #if the agent is from group 1, they cannot be influenced by :Influencers from group 2:
            g2 = filter(((k,v),) ->  v.group == 2 && v.infstatus == :RegularAgent, altergroup)
            temp2 = sample(collect(keys(g2)), counts2, replace = false)
            g2 = filter(((k,v),) -> k in temp2, g2)
        end
        
        if agent.group == 2

            #if the agent is from group 2, they cannot be influenced by :Influencers from group 1:
            g1 = filter(((k,v),) ->  v.group == 1 && v.infstatus == :RegularAgent, altergroup)
            temp1 = sample(collect(keys(g1)), counts1, replace = false)
            g1 = filter(((k,v),) -> k in temp1, g1)

            g2 = filter(((k,v),) ->  v.group == 2, altergroup)
            temp2 = sample(collect(keys(g2)), counts2, replace = false)
            g2 = filter(((k,v),) -> k in temp2, g2)
        end




        #helper for below: if there is no influencer, then this needs to presume there is no known influencer
        #however, if there is an influencer, below there will be a count for the third "group" (in-group influencers)
        #this is done for convenience when building the sampled alter group at the bottom of the function
        counts3 = 0 

    end

    #if the agent already has an influencer who is "stuck" to them
    if agent.infalterid != 0

        #the third group below is in-group social influencers (i.e. infstatus == :Influencer and, 
        #as we shall see shortly, agent.group==alter.group) these exist because we need a way
        #to increase the value of influencers at each time step provided they aren't discarded

        #agents can meet influencers with increasing probability
        if agent.group == 1
            ncontacts = sample([1,2,3], weights([(agent.w), (1-agent.w),(agent.w+agent.infvalue)]), ncontacts)
        elseif agent.group == 2
            ncontacts = sample([1,2,3], weights([(1-agent.w), (agent.w),(agent.w+agent.infvalue)]), ncontacts)
        end

        #counts3: counts for group 1 and group 2 social influencers that might exist in the alter group (in-group to the agent)
        #there may be none, but that's ok!
        counts3 = filter(((v),) -> v == 3, ncontacts)
        counts3 = length(counts3)
    
        #gather ncontacts, so we know how many are in group 1 and group 2
        counts1 = filter(((v),) ->  v == 1, ncontacts)
        counts1 = length(counts1)
        counts2 = filter(((v),) ->  v == 2, ncontacts)
        counts2 = length(counts2) 


        #filter all the agents in the model such that it excludes the current acting agent 
        #(i.e. agent cannot meet and infect itself)
        altergroup = filter(((k,v),) ->  k != agent.id, model.agents) 
        

        #filter altergroup into groups 1 and 2 (:RegularAgents only), and then randomly sample individuals 
        #from those groups based on the counts in the altergroup
        
        #because we are treating influencers as an imaginary group 3 for now for the purposes of sampling using probability (above), 
        # we filter groups 1 and 2 to only consist of :RegularAgents
        g1 = filter(((k,v),) ->  v.group == 1 && v.infstatus == :RegularAgent, altergroup)
        g1keys = sample(collect(keys(g1)), counts1, replace = false)
        g1 = filter(((k,v),) -> k in g1keys, g1)

        
        g2 = filter(((k,v),) ->  v.group == 2 && v.infstatus == :RegularAgent, altergroup)
        g2keys = sample(collect(keys(g2)), counts2, replace = false)
        g2 = filter(((k,v),) -> k in g2keys, g2)


        #if there is a count3 object with at least 1 influencer to draw (i.e. agent is in group 1, and there are influencers to be drawn)
        if counts3 ≥ 1

            #recall the key of the sticky influencer
            stickyinfluencerkey = agent.infalterid

            #filter so agent.group = group, and the sticky influencer is in the sample
            g3 = filter(((k,v),) ->  v.infstatus == :Influencer || v.infalterid == stickyinfluencerkey, altergroup)
            
            if length(g3) ≥ counts3
                g3keys = sample(collect(keys(g3)), counts3, replace = false) 
            else
                counts3 = length(g3)
                g3keys = sample(collect(keys(g3)), counts3, replace = false) 
            end

            if stickyinfluencerkey in g3keys
                #keep the influencer by randomly removing one and adding the sticky one
                g3 = filter(((k,v),) -> k in g3keys, g3)
            end

            #remove a random agent
            g3keys = sample(g3keys, length(g3keys)-1, replace = false)
            g3keys = push!(g3keys,stickyinfluencerkey)
            g3 = filter(((k,v),) -> k in g3keys, g3)  

        else
            g3 = Dict()
        end
  
    end

    #build the alter Dictionary
    if agent.infalterid != 0 #&& length(g3) > 0
        if length(g1) > 0 && length(g2) == 0 
            if length(g3) > 0
                alter = merge(g1,g3)
            else
                alter = g1
            end
            alter = Dict(alter)
            return alter

        elseif length(g1) == 0 && length(g2) > 0
            if length(g3) > 0
                alter = merge(g2,g3)
            else
                alter = g1
            end
            alter = Dict(alter)
            return alter

        elseif length(g1) > 0 && length(g2) > 0
            if length(g3) > 1
                alter = merge(g1,g2,g3)
            else
                alter = merge(g1,g2)
            end
            alter = Dict(alter)
            return alter
        
        else
            alter = Dict()
            return alter
        end


    elseif agent.infalterid == 0 #|| length(g3) == 0
        if length(g1) > 0 && length(g2) == 0 
            alter = g1
            alter = Dict(alter)
            return alter

        elseif length(g1) == 0 && length(g2) > 0
            alter = g2
            alter = Dict(alter)
            return alter

        elseif length(g1) > 0 && length(g2) > 0
            alter = merge(g1,g2)
            alter = Dict(alter)
            return alter
        else
            alter = Dict()
            return alter
        end

    end
end


## Social learning function ##
function learn!(agent, model)
    #Each agent has the opportunity to learn the protective behavior from the alter(s).
    #Agents can also spontaneously discard the behavior with low probability i.e. ≤ 0.1.

    #social influencers cannot learn from others
    if agent.infstatus == :Influencer && return
    end

    #Agents can randomly discard behavior throughout this process

    if agent.pbehavior == 1 && rand() ≤ 0.01
        agent.behavior == 0
    end

    #NOTE: this doesn't consider the agent's health status in learning how to behave --
    #might be an avenue for improvement!

    alter = sample_agents!(agent, model)

    #agent doesn't learn anything at all this round
    if length(alter) == 0 && return
    end

    #start with empty array of potential teachers drawn from the population
    teachers = []

    #If the agent has an influencer in their memory
    if agent.infalterid != 0 

        #sample social influencer or regular agent
        #minutely incrementally increasing weight of sampling influencer == 
        #increasingly sticky social influencer
        #it is nearly deterministic if agent.infvalue is high enough, 
        #though it's slow to get it to be a high value 
        #(and realistically maxes out around 5 extra person-weights, though this is very rare)
        #i.e., represents really committed influence media consumers.
        test_learnfrominfluencer = sample([:No,:Yes],weights([1,(1+agent.infvalue)]), model.properties[:nteachers])


        #draw the teacher 
        if test_learnfrominfluencer == [:Yes] #the teacher is the influencer the agent remembers in agent.infalterid
            teachers = filter(((k,v),) -> v.id == agent.infalterid, alter) 
            teachers = collect(values(teachers))
   
        end
        
        if test_learnfrominfluencer == [:No]
            alter = filter(((k,v),) -> v.id != agent.infalterid, alter) #remove the influencer
            alterkeys = rand(keys(alter), model.properties[:nteachers]) #draw a random pool of nteachers (in this case == 1) from the alters
            teachers = filter(((k,v),) -> k in alterkeys, alter)
            teachers = collect(values(teachers))
        end
    end

    if agent.infalterid == 0 #if agent has no influencer and is from either group

        #Below two lines commented out (to be deleted) bc all groups can meet influencers now.
        #if the agent is from the originating influence group, then they can learn from any agent
        #if agent.group == 1 

        ingroup_influencers = filter(((k,v),) -> v.group == agent.group && v.infstatus == :Influencer, alter)
        regular_agents  = filter(((k,v),) -> v.infstatus == :RegularAgent, alter)
        new_alter = merge(ingroup_influencers, regular_agents)
        alterkeys = rand(keys(new_alter), model.properties[:nteachers]) 
        teachers = filter(((k,v),) -> k in alterkeys, new_alter)
        teachers = collect(values(teachers))

            #if the agent is in either group, it can only meet RegularAgents and Influencers 
            #from its own group, but not outgroup Influencers


    end

    #if the alter group doesn't have the right kind of teacher based on above filtering, 
    #the agent doesn't learn (because the agent chooses their teacher -- and if there
    #isn't the right kind of teacher, a theoretical person the agent represents presumably 
    #wouldn't be interested)
    if length(teachers) == 0 && return
    end


    for teacher in teachers

        #currently, there is only 1 teacher per agent per timestep as specified above, 
        #but we specify this way to remain flexible in the future

        #create a new_encounter_memory for the agent's working decision-making memory re: behavior
        if teacher.group == agent.group

            if teacher.pbehavior == 1
                new_encounter_memory = ["ingroup", 1]
            elseif teacher.pbehavior == 0
                new_encounter_memory = ["ingroup", 0]
            end
        
        end

        if teacher.group != agent.group
            
            if teacher.pbehavior == 1
                new_encounter_memory = ["outgroup", 1]
            elseif teacher.pbehavior == 0
                new_encounter_memory = ["outgroup", 0]
            end

        end


        
        if length(agent.encounter_pool) == agent.encounter_baseline
            
            #if the agent.encounter_pool has met the threshold for decision-making re: 
            #adoption (i.e., agent.encounter_baseline), then forget the last encounter_memory
            #in the list, and add the new encounter_memory to the agent.encounter_pool
            #NB: this allows for dynamic changes in decision-making over time 
            #(a fundamental requirement of the model)
            current_pool = agent.encounter_pool #separate it out -- not necessary, but safe and explicit
            current_pool = pushfirst!(current_pool,new_encounter_memory) # add the new memory to the beginning of the current pool of encounters
            agent.encounter_pool = current_pool[1:agent.encounter_baseline] # keep the n = encounter_baseline entries for new memory pool (i.e. drop the last and oldest one)
        
        end
        
        if length(agent.encounter_pool) < agent.encounter_baseline

            #if the agent.encounter_pool is still smaller than the agent.encounter_baseline, 
            #just add the encounter_memory to continue to build alter observations towards the baseline
            agent.encounter_pool = pushfirst!(agent.encounter_pool,new_encounter_memory)
        
        end
        
        if length(agent.encounter_pool) == 0
        
            #if the agent.encounter_pool is an empty array, 
            #replace it with the encounter_memory to start remembering alter observations
            agent.encounter_pool = new_encounter_memory
        
        end


        #this is placed prior to the next part to assure that agents can only be updated to 
        #:learning at the time step following the one during which they reach their encounter_baseline
        if length(agent.encounter_pool) == agent.encounter_baseline
            
            agent.observelearn = :learning
        
        end

        if length(agent.encounter_pool) > agent.encounter_baseline
            
           print("length of agent's encounter pool is greater than their encounter baseline")
           break
        
        end


        #count the observed alter-agents to determine whether the agent is prepared to learn.
        #if the agent's encounter baseline has not been achieved (as of the previous time step), no behavioral adoption testing (learning) (but can keep influencer).
        #if the agent's encounter baseline has been achieved (as of the previous time step), test behavioral adoption (try to learn).


        if agent.observelearn == :learning

            #If the agent.observelearn == :learning, we must count the number of ingroup and outgroup agents who have adopted or not adopted the behavior.

            counts = countmap(agent.encounter_pool)

            observe_inbehavior_yes = filter(((k,v),) -> k == ["ingroup", 1],counts)
            observe_inbehavior_yes = length(observe_inbehavior_yes)

            observe_inbehavior_no = filter(((k,v),) -> k == ["ingroup", 0],counts)
            observe_inbehavior_no = length(observe_inbehavior_no)

            observe_outbehavior_yes = filter(((k,v),) -> k == ["outgroup", 1],counts)
            observe_outbehavior_yes = length(observe_outbehavior_yes)

            observe_outbehavior_no = filter(((k,v),) -> k == ["outgroup", 0],counts)
            observe_outbehavior_no = length(observe_outbehavior_no)

            #we calculate the weighted probability of the agent learning, 
            #using the in-group observations (weighted as 1),
            #and the outgroup observations (weighted by 1-aversion value)
            #reasoning: conformity theory, where the agent decides based on the mean behavior of 
            #the observed group

            #this is potentially changing based on the current agent's encounter_pool during this step
            pr_adopt_behavior = ((observe_inbehavior_yes) + (observe_outbehavior_yes * (1-model.properties[:aversion])))/(agent.encounter_baseline)
            
            #calculated explicitly:
            pr_not_adopt_behavior = ((observe_inbehavior_no) + (observe_outbehavior_no * (1-model.properties[:aversion])))/(agent.encounter_baseline)

            #behavioral adoption decision
            adoption_decision = sample([:adopt_behavior,:donot_adopt],weights([pr_adopt_behavior,(pr_not_adopt_behavior)]), 1)

            if adoption_decision == [:adopt_behavior]
                agent.behavior = 1

            elseif adoption_decision == [:donot_adopt]
                agent.behavior = 0
            end
          
        end


    #if the teacher is an influencer (a teacher-influencer) and the agent and the teacher are from the same group

        if teacher.infstatus == :Influencer && agent.group == teacher.group #implicit, but just in case be explicit

            #if the agent has no influencer in memory, remember the teacher-influencer
            #(this gets tested for "Keep" or "Discard" for staying power below)
            if agent.infalterid == 0
                agent.infalterid = teacher.id
                #print("remembered influencer")
            end

            #if the agent has an influencer in memory
            if agent.infalterid !=0
                
                #if that influencer id in memory is the same as the teacher-influencer, 
                #keep it for now (again, gets tested below)
                if teacher.id == agent.infalterid
                    agent.infalterid = agent.infalterid
                    #print("kept influencer")
                end
                
                #otherwise if the teacher-influencer is not the same influencer, flip a coin to pick one (weighted)
                if teacher.id != agent.infalterid
                    weightedcoin = sample([:newinfluencer,:previousinfluencer],weights([1,(1+agent.infvalue)]), 1)
                    
                    if weightedcoin == [:newinfluencer]
                        agent.infalterid = teacher.id
                        agent.infvalue = 0 #this gets updated to value z if kept in next test in function
                    end
                    #implicit here: if weightedcoin == :previousinfluencer, then it's kept
                end

            end
        
        end
    end

    #If the agent has a social influencer recorded, 
    #test to see if the agent keeps the social influencer or discards 
    #(i.e. treats them like any other teacher agent)
    weightedcoin = sample([:testdiscard,:testkeep],weights([1,(1+agent.infvalue)]), 1)

    if agent.infalterid != 0 && weightedcoin == [:testkeep]
            agent.infkeep = :Keep
            agent.infvalue += model.properties[:z]

    elseif agent.infalterid != 0 && weightedcoin == [:testdiscard]
            agent.infkeep = :Discard
            agent.infvalue = 0
    end

    #this step must come before the following step where infvalues are updated upon discard
    if agent.infalterid == 0 
        if weightedcoin == :testkeep && teacher.infstatus == :Influencer #agent.infvalue should be == 0 so this should work fine
            for i in 1:length(teacher) #NB: this is really only going to work if there's an nteach value of 1!
                agent.infalterid = teacher[i].id
                agent.infvalue = model.properties[:z] #seed
                agent.infkeep = :Keep
            end
        else
            agent.infalterid = 0
            agent.infvalue = 0
            agent.infkeep = :null

        end
    end

    if agent.infkeep == :Discard
        agent.infvalue = 0
        agent.infalterid = 0
    end
end



## Function: Disease transmission ##
function transmit!(agent, model)
    #If the agent is susceptible, the agent meets the alter list for the agent 
    #in that time step, and we test whether they become infected or not

    #social influencers cannot become infected, return
    if agent.infstatus == :Influencer && return
    end

    #If agent is not susceptible, return
    if agent.pstatus != :S && return
    end
    
    alter = sample_agents!(agent, model)

    #agent doesn't change status at all this round: didn't meet anybody
    if length(alter) == 0 && return
    end

    #Collect all the alter cases, and filter them to those that are infected
    alter_infect = filter(((k,v),) ->  v.pstatus == :I, alter)

    #Iterate over the number of infected cases in the alters to see if infection occurs
    #i.e., if an agent meets infected agents, their probability of infection presumably goes up
    #this handles it case-by-case to mimic real-world interactions == repeated trials
    
    #NB: NOT iterating over the actual infected cases themselves (can change this later if needed!) 
    #reasoning: there's nothing overtly special about any one agent's ability to infect another 
    #agent in this specification of the model, so it's the same repeated test for agent vs. infected alters

    for i in 1:length(alter_infect)

        if agent.status == :I && break
        end
       
        #If the agent did the behavior (==1), 
        #test the joint probability: probability of an infection occurring 
        #given the probability that the protective behavior fails (when behavior == 1)
        #Probability of becoming infected is a combination of β and an agent's personal risk:
        #if an agent does not protect themselves, their personal risk == 1. So below,
        #elseif agent.behavior == 0 && (rand() ≤ model.properties[:β]) is testing implicitly:
        #elseif agent.behavior == 0 && (rand() ≤ model.properties[:β] * 1)
        #whereas agent.behavior == 1 && (rand() ≤ (model.properties[:β] * (1-model.properties[:b_protect])))
        #is actually testing the personal risk of a masked person, where at 0.85 efficacy, we assume the inverse
        #is the personal risk: 1-0.85 = 0.15, so we're testing β * 0.15 (the mask is very effective this way,
        #as it should be with this parameterization)
        if agent.behavior == 1 && (rand() ≤ (model.properties[:β] * (1-model.properties[:b_protect])))
            agent.status = :I #infection occurs
            #count a new infection
            agent.newly_infected = 1
        #If the agent did not do the behavior (==0), 
        #test the probability of an infection occurring given no protective behavior (i.e. behavior == 0)
        elseif agent.behavior == 0 && (rand() ≤ model.properties[:β])
            agent.status = :I  #infection occurs
            #count a new infection
            agent.newly_infected = 1
        else
            agent.status = :S #no infection occurs
        end
    end
end



## Function: Test agent recovery ##
function recover!(agent, model)
    #A function to stochastically test whether infected agents recover during the time step
    if agent.pstatus != :I && return
    end

    if rand() ≤ model.properties[:γ]
        agent.status = :R
    end

end


## Function: specify each step of the model ##
#A function to apply the series of functions in the time step to each agent, 
#one at a time, and then to the whole model
function agent_step!(agent, model)

    #If we assume all agents must use previous step's data to select options for this step
    #then we must assume that we need to save this information and work from it to establish
    #the current step's strategy

    #if an agent was previously counted as newly_infected == 1, reset value to 0 so that we can sum
    #across this by step and group later to get the group-specific new infection CDF easily
    if agent.newly_infected == 1
        agent.newly_infected = 0
    end

    #Social learning stage
    #Recall previous behavior before behavior is updated
    agent.pbehavior = agent.behavior
    #Recall previous status: helper
    agent.pstatus = agent.status
    learn!(agent, model)
    
    #Epidemiological stage
    transmit!(agent, model)
    recover!(agent, model)

end
#= 

## Run the model ##
abm_model = init_model(β,c,γ,N,I0,b_protect)
#test to make sure there are seeded early adopters
test = filter(((k,v),) -> v.behavior == 1, abm_model.agents)
test = filter(((k,v),) -> v.status == :I, abm_model.agents)
test = filter(((k,v),) -> v.infstatus == :Influencer, abm_model.agents)
test = filter(((k,v),) -> v.group == 1, abm_model.agents)
test = filter(((k,v),) -> v.group == 2, abm_model.agents)


## Collect group and status into a df and combine ##
#I think the fastest scheduler is fine for this

#In the future, if we want to save the agent.ids and agent.infstatus to track which agents are most influential (and if they're :Influencers),
#this can be done by changing the encounter_memory Dict to include those data. I simply haven't got around to it yet.

print("start: ", now())
abm_data, _ = run!(abm_model, agent_step!, nsteps; adata = [:group,:status,:behavior,:learntest,:infvalue,:infstatus])
print("end: ", now())

abm_data_status = combine(groupby(abm_data, [:step,:group,:status]), nrow => :n_status) # first(abm_data_status, 5)
last(abm_data_status, 5)

abm_data_behavior = combine(groupby(abm_data, [:step,:group,:infstatus,:behavior]), nrow => :n_behavior) #
first(abm_data_behavior, 5)
last(abm_data_behavior, 5)

#abm_data_learntest = combine(groupby(abm_data, [:step,:id,:group,:behavior,:learntest,:infvalue]), nrow => :n_learntest) #

## Export data as CSV files ##

if model_with_influencers == true

    CSV.write("output/abm_results/m5_with_influencers_complexcontagion_aversion/abm_data_raw.csv", abm_data)
    CSV.write("output/abm_results/m5_with_influencers_complexcontagion_aversion/abm_data_status.csv", abm_data_status)
    CSV.write("output/abm_results/m5_with_influencers_complexcontagion_aversion/abm_data_behavior.csv", abm_data_behavior)
    #CSV.write("output/abm_results/m5_with_influencers_complexcontagion_aversion/abm_data_learntest.csv", abm_data_learntest)

else

    CSV.write("output/abm_results/m5_no_influencers_complexcontagion_aversion/abm_data_raw.csv", abm_data)
    CSV.write("output/abm_results/m5_no_influencers_complexcontagion_aversion/abm_data_status.csv", abm_data_status)
    CSV.write("output/abm_results/m5_no_influencers_complexcontagion_aversion/abm_data_behavior.csv", abm_data_behavior)
    CSV.write("output/abm_results/m5_no_influencers_complexcontagion_aversion/abm_data_learntest.csv", abm_data_learntest)

end =#

#Next steps: we load up csvs in R and visualize with tidyverse & ggplot2 (see folder: r_code)
#In the future: R macro?












