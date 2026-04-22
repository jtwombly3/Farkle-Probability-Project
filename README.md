# Farkle-Probability-Project
Calculating the probability of certain farkle roles using simulations. Coded in R Studio using shiny package.

# Background
Oh man.
This was pretty interesting because it was my first time using conditional panels.
I think it gives a little structure to the shiny app and keeps it from looking cluttered.

On the server side, the OAK probabilities and the unique type probabilites were relatively
easy to calculate because the logic was simple using any/all operators.
Farkle was pretty difficult however.
The conditions and the mutual exclusivity threw me through a loop.
The "|" operator was the saving grace.
Also, building a function that was adaptable to farkles with different dice numbers
was critical so that using if statements was not necessary.
To be honest, if statements give me tunnel vision. When I see an if statement I want to write
one million of them and hardcode every contingency.

Main sources for this are r shiny gallery, r shiny postit, r documentation, and chat gpt
Just for my own ego, know that I tried to minimize my use of chatgpt and only used it when I was legitimately flummoxed
Which was defining the function for farkle_test and fighting with if statements which ended up not being used
