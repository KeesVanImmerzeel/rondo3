
# Analytical calculation of groundwater flow in a single aquifer under circular shaped polders.

C.H. van Immerzeel

6/4/2021

In the app Rondo3 two circular shaped polders are defined. The resulting heads in the aquifer below these polders is calculated, as well as the lateral discharge in this aquifer and the seepage to of from the polders.

An R-package (see below) may be used in case more polders should be taken into account.In that case, you need to create an R-script.

![](https://user-images.githubusercontent.com/16401251/113734274-7e9b5980-96fb-11eb-910d-4ddab96cf72b.PNG)
![](https://user-images.githubusercontent.com/16401251/113697604-b55d7980-96d3-11eb-8f54-93049840d2d8.PNG)
![](https://user-images.githubusercontent.com/16401251/113697646-c5755900-96d3-11eb-8b47-5033a3722d09.PNG)


## Input
- Radius of the polders (m).

Per polder and surrounding area:

- kD: Transmissivity (m2/d);

- c: Hydraulic resistance of top layer (d);

- H: Polder level (m).

## Output
At radius x:

- Head: Head in aquifer (m);

- Q: Lateral discharge (m3/d);

- Seepage: Seepage intensity (m/d).


## Link to the app

<https://sweco.shinyapps.io/Rondo3/>

## Source code
<https://github.com/KeesVanImmerzeel/rondo3>

## R-package
<https://github.com/KeesVanImmerzeel/rondo>

## References
- Immerzeel, C.H. 1990. Berekening van de stationaire grondwaterstroming van hogere naar lagere (ronde) gebieden.
- Comm. voor Hydrologisch Onderzoek T.N.O. (1964). Steady flow of ground water towards wells. Verslagen en mededelingen No. 10.
- <https://edepot.wur.nl/184522>
