#Life Insurance Premium Modelling

This project uses real mortality data from the UK Office for National Statistics to estimate the fair annual life insurance premium for individuals at each age. The model calculates the expected years remaining for a person of a given age and then uses net present value (NPV) methods to determine the level premium required for an insurance company to break even.

Key features of the project include:

Monte Carlo simulation to estimate life expectancy based on age-specific mortality rates.

A custom-built variable-probability geometric-like distribution, designed to realistically simulate age of death by allowing the probability of death to change each year.

Data visualization using base R plotting tools to show trends in life expectancy and premium cost by age.

A more conservative nested simulation model that accounts for adverse longevity risk by considering the worst-case (longest-lived) scenario from multiple life paths, which may be useful in capital modelling and stress testing contexts.
