

### Solve carbon fluxes

The simulation be can run with and without a resolution of carbon fluxes in the plant. 

- **Without**, the growth of each organ will be set at its maximal value (potential growth)
- **With**, carbon will be produced by each segment, then allocated to the different plant organs. The growth of each organ will then be modified if the amount of carbon allocated does not meet the demand. 

#### Carbon production

Carbon production is based on the Farquhar model. A version of the model can be [found online here](http://biocycle.atmos.colostate.edu/shiny/photosynthesis/)

#### Carbon demand 

The carbon demand is the sum of the *growth* demand and the *maintenance* demand for each organ. Both value are computed based on the dry mass of the organ (based on Drouet & Pagès 2003). 

#### Carbon allocation

The allocation of the available carbon between all plant articles follows precise priority rules. Carbon is used in priority to meet the whole plant maintenance demand, considered as an obliga- tory cost (Drouet and Pagès, 2003; Postma and Lynch, 2011). The remaining C is divided between the root and the shoot according to a root- to-shoot C allocation ratio. In the shoot, priority is given to the leaf growth over the stem, while in the roots, priority is defined based on the potential growth rate of each root (sink term). Once this allocation is performed, a growth satisfaction coefficient is computed for every article (between 0 and 1). This coefficient will be used (1) to define the actual growth of the different articles

#### References

- Drouet J-L, Pagès L. GRAAL: a model of GRowth, Architecture and carbon ALlocation during the vegetative phase of the whole maize plant. Ecol Modell. 2003;165: 147–173. doi:10.1016/S0304-3800(03)00072-3
- Lobet G, Pagès L, Draye X. A modeling approach to determine the importance of dynamic regulation of plant hydraulic conductivities on the water uptake dynamics in the soil-plant …. Ecol Modell. Elsevier; 2014; Available: https://www.sciencedirect.com/science/article/pii/S0304380013005735
- Postma JA, Lynch JP. Theoretical evidence for the functional benefit of root cortical aerenchyma in soils with low phosphorus availability. Ann Bot. Annals Botany Co; 2011;107: 829–841. Available: http://aob.oxfordjournals.org/cgi/doi/10.1093/aob/mcr089