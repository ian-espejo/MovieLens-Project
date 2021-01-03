# MovieLens-Project
This project creates a movie recommendation system using the 10M version of the MovieLens dataset. In addition to movie and user effects, 'title' and 'timestamp' variables are used to extract movies' release years, and calculate a new rate timestamp indicator, incorporating all four effects in the final model, which are also regularized by a tuning parameter of 0.5 chosen through cross-validation. The final model reaches a residual mean squared error (RMSE) ~ 0.8647 when employed on the Validation Set, which is lower than the project's target.
