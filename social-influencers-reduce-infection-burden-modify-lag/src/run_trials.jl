using Distributed
using Dates
using UUIDs: uuid4

using DrWatson
quickactivate("..")

# Set up DrWatson to include vectors in autogen save names.
da = DrWatson.default_allowed(Any)
DrWatson.default_allowed(c) = (da..., Vector)

using ArgParse
using CSV
using Comonicon
using JLD2

include("./experiment.jl")

s = ArgParseSettings()

function parse_cli()

    @add_arg_table s begin

        "datadirname"
            help = "Subdirectory of data/, where to save output data"
            arg_type = String
            required = true
        
        "--aversion"
            help = "Value used to down-weight outgroup agents' observed behaviors in behavioral adoption/learning phase"
            arg_type = Float64
            default = 0.0

        "--g1_influencer_message"
            help = "Group 1 influencer message: either positive (1) or negative (0)"
            arg_type = Int
            default = 0
        
        "--g2_influencer_message"
            help = "Group 2 influencer message: either positive (1) or negative (0)"
            arg_type = Int
            default = 0
        
        "--w1"
            help = "Group 1 homophily value used to sample potential teachers (from which 1 teacher is randomly selected for learning/infection)"
            arg_type = Float64
            default = 0.5
        
        "--w2"
            help = "Group 2 homophily value used to sample potential teachers (from which 1 teacher is randomly selected for learning/infection)"
            arg_type = Float64
            default = 0.5
        
        "--ninfluencers"
            help = "Number of influencers in the bidirectional influence model: either 0 (no influence), or 20 (10 influencers in both group 1 and group 2)"
            arg_type = Int
            default = 20

        "--f"
            help = "Number of other agents an agent must observe before learning"
            arg_type = Int
            default = 3
        
        "--nreplicates"
            help = "Number of replicates"
            arg_type = Int
            default = 10

        "--nagents"
            help = "Number of agents (Regular Agents, not Influencers)"
            arg_type = Int
            default = 1000
            
    end

    return parse_args(s)
end



function run_trials(nreplicates = 10; 
                    outputfilename = "trials_output.jld2", 
                    experiment_kwargs...)

    tic = now()

    println("Starting trials at $(replace(string(tic), "T" => " "))")

    # XXX Awkward stuff due to mixing around positional argument as either
    # nagents or nreplicates.
    kwargs_dict = Dict(experiment_kwargs)
    nagents = pop!(kwargs_dict, :nagents)
    #kwargs_dict[:nreplicates] = nreplicates

    result_df = social_influence_experiment(nagents; kwargs_dict...) #try Julia 1.7

    CSV.write(outputfilename, result_df)

    trialstime = Dates.toms(now() - tic) / (60.0 * 1000.0)

    println("Ran expected payoffs trials in $trialstime minutes")
end

function main()
    parsed_args = parse_cli()

    # Create job id for unique filename.
    parsed_args["jobid"] = string(uuid4())
    println(parsed_args)

    datadirname = pop!(parsed_args, "datadirname")
    nameargs = copy(parsed_args)

    outputfilename = joinpath(datadirname, savename(nameargs, "csv"))

    nreplicates = pop!(parsed_args, "nreplicates")

    # Don't need to pass this job ID to experiment.
    pop!(parsed_args, "jobid")

    # Need keys to be symbols for passing to run_trials function.
    pa_symbkeys = Dict(Symbol(key) => value for (key, value) in parsed_args)

    run_trials(nreplicates; outputfilename, pa_symbkeys...)

end


main()

# Tell us it's done, so I can do other stuff while I wait
#run(`say \"Complete\"`)

