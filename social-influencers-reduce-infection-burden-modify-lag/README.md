# Social influencers reduce infection burden and modify epidemic lag in group-structured populations 


## Technical Specifications

This analysis is primarily built in the Julia programming language using Agents.jl to implement and run agent-based models. The visualization of the results is completed in R programming language using primarily ggplot2.


This code base is using the [Julia Language](https://julialang.org/) and
[DrWatson](https://juliadynamics.github.io/DrWatson.jl/stable/)
to make a reproducible scientific project named
> social_influence_epi

It is authored by Aja Sutton.

To (locally) reproduce this project, do the following:

0. Download this code base. Notice that raw data are typically not included in the
   git-history and may need to be downloaded independently.
1. Open a Julia console and do:
   ```
   julia> using Pkg
   julia> Pkg.add("DrWatson") # install globally, for using `quickactivate`
   julia> Pkg.activate("path/to/this/project")
   julia> Pkg.instantiate()
   ```

This will install all necessary packages for you to be able to run the scripts and
everything should work out of the box, including correctly finding local paths.

You may notice that most scripts start with the commands:
```julia
using DrWatson
@quickactivate "social_influence_epi"
```
which auto-activate the project and enable local path handling from DrWatson.

