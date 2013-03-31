# `sample`

`sample` provides a type class and a handful of common instances for taking random samples of containers of data. Sampling can be done with or without a `MonadIO` instance; without one, a `RandomGen` instance must be provided as a source of randomness.
