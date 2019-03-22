FoRTE baseline
================
Alexey N. Shiklomanov
06 June, 2019

# Introduction

Temperate forests in the Upper Midwest are important. {C sequestration}.
{Other ecosystem services}. However, the fate of these forests is highly
uncertain. The natural ecological succession of these forests has been
significantly altered by the interactions of a significantly different
and rapidly changing climate, and novel disturbance regimes under which
some natural disturbances (e.g. fire) are suppressed while others
(e.g. harvest; pests and pathogens) may be becoming more frequent.
<!-- TODO refs --> {UMBS ecological background/natural history?}
Therefore, predictions about the future of these forests based on past
analogs may not be reliable.

Instead, more reliable predictions can be obtained by using dynamic
vegetation models that explicitly represent process involved in forest
growth and mortality. Vegetation models fall broadly along the following
spectrum of complexity. On one side are relatively simple “Big leaf”
models—such as PNET, SiBCASA, and Biome-BGC
<!-- TODO refs; more examples? -->—in which the vegetation at a
particular location consists essentially of a single large “plant” whose
characteristics are the (weighted) average of all the vegetation at that
site. <!-- TODO refs --> These models were designed originally to
simulate the land surface boundary condition for atmospheric general
circulation models, though similar models continue to be applied
successfully to simulate biogeochemical processes in many ecosystem
science contexts. <!-- TODO refs --> On the other end of the spectrum
are “individual-based” models (a.k.a. “gap models”), which explicitly
simulate multiple indivduals competing for resources at a single site
(Shugart et al. 2015). <!-- TODO other refs? --> Examples of such models
are LANDIS <!-- TODO ref !--> and UVAFME. <!-- TODO ref -->
<!-- TODO Other examples? --> Because these models can explicitly
represent inter-specific differences in plant productivity, resource
allocation, and stress tolerance (among others), as well as the
competition that emerges out of these differences, they may be better
able to represent changes in ecosystem-scale processes, especially in
no-analog conditions (Purves and Pacala 2008). Moreover, these models’
predictions of species composition and ecosystem structure are relevant
outside of carbon cycle science, particularly for forest management and
wildlife conservation. <!-- TODO refs --> However, these benefits of
individual-based models come at the cost of much higher data and
computational requirements, which effectively restrict their application
to individual, well-studied sites.
<!-- TODO refs. Also, is this actually true? -->

A key challenge of vegetation modeling research over the last several
decades has been the development of approaches that capture the emergent
biogeochemical and ecological outcomes of interactions between
individual plants without the need for explicitly simulating each
individual (Purves and Pacala 2008). One such approach is the “ecosystem
demography” approach, which uses a system of partial differential
equations to simulate the emergent mean behavior of “cohorts” of
individuals of similar composition, size, and age (Moorcroft et al.
2001). <!-- TODO Run this description by Mike? --> The original
“Ecosystem Demography” (ED) model (Moorcroft et al. 2001) was enhanced
by Medvigy et al. (2009) (as ED2) with fully-coupled leaf physiology,
canopy radiative transfer, and micrometeorology, and each of these
components have been further refined over the last decade (culminating
in ED2.2; Longo et al. 2019b). Various versions of the ED model have
been validated in boreal (Medvigy and Moorcroft 2011), temperate
(Medvigy et al. 2009, Dietze et al. 2014, Raczka et al. 2018), and
tropical biomes (Moorcroft et al. 2001, Longo et al. 2019a), and have
been applied in:

  - paleoecological studies (Rollinson et al. 2017);
  - free-air CO<sub>2</sub> enrichment studies (De Kauwe et al. 2013,
    Miller et al. 2015);
  - estimation of potential carbon stocks (Hurtt et al. 2004)
  - vegetation-climate feedbacks {Swann, etc} <!-- TODO -->

Vegetation model projections are inherently uncertain, and this
uncertainty stems from several sources. *Driver* uncertainty refers to
uncertainty in data about processes that are essential to the system in
question but are not represented by the model (e.g. weather and climate
for vegetation models). *Initial condition* uncertainty arises from the
fact that models have to start somewhere, and the exact conditions at
the place and time simulations begin are uncertain. *Process* or
*structural* uncertainty arises because vegetation models necessarily
only represent a subset of all processes involved in plant biology, and
that the process that are included can usually be represented in
multiple different ways (i.e. which processes are included, and how they
are represented). Finally, *parameter* uncertainty arises because of
natural variability and imperfect calibration of the above process
representations. <!-- TODO This whole paragraph is awkward -->

Several recent studies have looked at parameter uncertainty in ED2.
Dietze et al. (2014) evaluated which ED2 parameters contributed the most
to uncertainties in ED2 simulations of productivity across multiple
biomes in North America at 1, 5, and 10 year time scales. They found
that the parameters contributing the most to model uncertainty across
all biomes were those related to growth respiration, mortality, stomatal
conductance, and water uptake. However, they also found that despite
some common patterns, parameter uncertainty varied with biome. More
recently, a similar analysis evaluated parameter uncertainty over 100
year time scale at the Willow Creek Ameriflux site, and found that the
most important parameters to model uncertainty were quantum efficiency
of photosynthesis, leaf respiration, and soil-plant water transfer
(Raczka et al. 2018). Another recent study assessed parameter
uncertainty in ED2 as part of its overall goal of parameterizing the
model for sagebrush (Pandit et al. 2018). They found that the most
important parameters were specific leaf area, the maximum rate of carbon
fixation (V<sub>cmax</sub>), slope of stomatal conductance, the
fine-root to leaf carbon ratio, and the fine root turnover rate. Another
study looked specifically at contributions to uncertainty from
parameters related to canopy radiative transfer, and found that
parameters related to both canopy structure and leaf optical properties
had a large (\>20%) impact on predictions of net primary productivity
(<span class="citeproc-not-found" data-reference-id="viskari_2019_influence">**???**</span>).
The variability in results across these studies demonstrates that
parameter contributions to ED2 uncertainty are highly variable, and that
additional parameter uncertainty studies are needed.

While parameter uncertainty ED2 has been previously evaluated, less work
has been done on structural uncertainty. Numerous vegetation model
intercomparison studies have demonstrated that different models produce
significantly different projections of overall land carbon sequestration
(Friedlingstein et al. 2006, 2014), response to CO<sub>2</sub>
fertilization (REF - Medlyn?), high-latitude warming (Rogers and
Serbin?), {others…}. <!-- TODO --> These differences have been
attributed to differences in model representations of key processes,
including canopy radiative transfer (Fisher et al. 2017) and
photosynthesis (Rogers et al. 2016). <!-- TODO More here? --> However,
the extent to which specific processes contribute to model uncertainty
is difficult to evaluate from model intercomparisons because different
models are different from each other in too many different ways. In this
study, we evaluate structural uncertainty more precisely/directly by
selectively toggling and switching individual processes/model components
in ED2 while keeping everything else constant.

The focus of this study is on the interaction between parametric and
structural uncertainty in the ED2 model. Driver uncertainty is outside
the scope of this study, and initial condition uncertainty is minimized
by our experimental design, which is conditioned on a specific initial
condition (near bare ground). Our study is organized around the
following guiding questions: (1) Which processes related to light
utilization are most important to accurately modeling community
succession and C cycle dynamics in temperate forests of the Upper
Midwest? (2) What is the cost of considering these processes, in terms
of additional parameteric uncertainty? (3) What are the relative
contributions of parameteric uncertainty (data limitation)
vs. structural uncertainty (theoretical limitation?)? To answer these
questions, we ran ED2 ensemble simulationes with a factorial combination
of submodels related to radiative transfer formulation (two-stream
vs. multiple scatter), horizontal competition (finite canopy radius
vs. complete shading), and trait plasticity (whether or not SLA and
Vcmax vary with light level) We then used a sensitivity and variance
decomposition analysis to evaluate the contribution of parameter
uncertainty for each model configuration.

## Old introduction outline

  - Why we need models?
      - Forests are important, for C sequestration, other ecosystem
        services
      - Policy decisions related to forests depend on future projections
      - We need models to make future projections
  - What kinds of models are out there?
      - Traditional: “Big leaf” models
          - Needed to be simple to run on old computers
          - Don’t need as much data
          - …but they fail to capture important processes, which matter
            not only for accurate predictions about C cycle, but also
            because they are processes we care about (habitat, forestry,
            tourism, etc.)\!
      - Individual-based models
          - Much more detailed, but harder to run, and need much more
            data.
          - Trade-off between complexity and accuracy.
  - Cohort-based models like ED2 as a middle ground?
      - Core ED concept (Moorcroft et al. 2001)
      - ED2 developments – (Medvigy et al. 2009, Medvigy and Moorcroft
        2011)
      - Most recent version – (Longo et al. 2019b)
      - Validation/applications of the ED model:
          - (Rollinson et al. 2017) – ED2 paleoclimate
          - (Miller et al. 2015) – ED2 at Duke FACE
          - (De Kauwe et al. 2013) – ED2 at Duke and Oak Ridge FACE
          - (Longo et al. 2019a) – Model evaluation in Tropics
  - How much complexity do we need to accurately model ecological
    dynamics?
      - Lots of existing work on parameteric uncertainty
          - (Dietze et al. 2014)
          - (Raczka et al. 2018)
          - (Pandit et al. 2018)
      - Less on structural uncertainty – i.e. not just what coefficients
        do we use, but what *equations*, and more generally, what
        *processes* do we even consider?
  - What are the processes that drive succession of ecological
    communities?
      - Competition for light (Bazzaz 1979)
      - Others? Leaning towards omitting because factorial combination
        becomes too large.
          - Competition for water?
          - Competition for nutrients? (ED’s N model isn’t very good.
            Alternatives are being developed, but not in ED mainline
            yet.)
  - Brief review of community/ecosystem ecology of the Upper Midwest
      - UMBS is a useful site for testing these theories because of its
        natural history.
      - Around 1900, mature forest dominated by white (*Pinus strobus*)
        and red pine (*Pinus resinosa*), hemlock (*Tsuga canadensis*),
        and sugar maple (*Acer saccharum*).
      - But, repeat harvest and burning between 1880 and 1920 (Kilburn
        1960) led to replacement by early successional aspen (*Populus
        grandidentata* and *Populus tremuloides*), birch (*Betula
        papyrifera*), and cherry (*Prunus pensylvatica*)
        (<span class="citeproc-not-found" data-reference-id="barnes_1981_michigan">**???**</span>,
        Gates 1930, Friedman and Reich 2005, Bergen and Dronova 2007).
      - Such aspen-dominated secondary forests are sustained by
        background levels of disturbance, particularly fire (Gates 1930)
      - However, due to aggressive fire suppression in the US, these
        forests are now undergoing succession, primarily to
        mid/late-successional hardwood (typified by sugar maple and
        American beech, *Fagus grandifolia*) or upland conifer (typified
        by red and white pine) (Bergen and Dronova 2007).
      - The combination of stand-replacing disturbance \~100 years ago
        and suppressed disturbance since then makes this an ideal site
        for testing a vegetation model run from bare ground without
        having to prescribe disturbance.
  - Guiding questions:
      - Which of these processes do we need to be able to accurately
        model what happens (“community succession and C cycle dynamics”)
        to Midwestern forests?
      - What is the cost of considering these processes, in terms of
        additional parameteric uncertainty?
      - What are the relative contributions of parameteric uncertainty
        (data limitation) vs. structural uncertainty (theoretical
        limitation?)?
  - Study overview:
      - ED with factorial combination of submodels related to radiative
        transfer formulation (two-stream vs. multiple scatter),
        horizontal competition (finite canopy radius vs. complete
        shading), and trait plasticity (whether or not SLA and Vcmax
        vary with light level)
      - Ensemble analysis to look at parameter uncertainty

# Methods

## Site description

  - Secondary successional forest – bigtooth aspen (*Populus
    grandidentata*), northern red oak (*Quercus rubra*), red maple
    (*Acer rubrum*), paper birch (*Betula papyrifera*), eastern white
    pine (*Pinus strobus*)
  - Average age – 95 years (2013)
  - NEP in UMBS tower is 1.58 MG C ha<sup>-1</sup> yr<sup>-1</sup>
    (0.80-1.98) (1999-2006), but with landscape variation (REF?)
  - Heavily logged in late 1800s and early 1900s, disturbed by fire
    until 1923 (REF)
  - Present day composition typical of upper Great Lakes region
  - Previously, the site of the FASET experiment (Gough et al. 2008,
    2013)
  - UMBS is 87% upland and 13% wetland (Bergen and Dronova 2007). We
    focus on the upland ecosystem, which has the following
    characteristics (from Bergen and Dronova (2007)):
      - 20.4% moraine, 37.8% high outwash plain, 31.3% low outwash
        plain, 5.7% lake-margin terrace, 3.6% ice-contact, 1.2% lowland
        glacial lake
      - 60.9% aspen-dominated, 16.6% northern hardwood (*Acer
        saccharum*, *Acer rubrum*, *Fagus grandifolia*, *Tilia
        americana* (basswood), *Betula alleghaniensis* (yellow birch),
        *Fraxinus americana* (white ash), *Tsuga canadensis*), 13.3%
        upland conifer (*Pinus strobus*, *Pinus resinosa*)
      - 1.6% northern red oak (*Quercus rubra*)
      - 60.9% transitioning from aspen to next succession, of which:
        55.8% to northern hardwood, 34.7% to upland conifer, 3.8% to
        northern red oak, 2.6% to northern red oak-red maple, 3.1% to
        upland conifer-northern hardwood

## ED2 model description

The Ecosystem Demography Model, version 2.2 (ED2). Default configuration
is described in (Longo et al. 2019b).

In this study, we run a factorial experiment of ED2 with the following
features enabled or disabled:

  - Radiative transfer
      - Two-stream model (ED default)
          - Same general approach as in CLM 4.5 (Oleson et al. 2013),
            but rather than solving for just two layers (sunlit and
            shaded), solve for each canopy layer.
          - Set up the two-stream equations for each layer as a linear
            system and solve the entire set of equations numerically
          - Similar to approaches used in other models: JULES (Mercado
            et al. 2007), ORCHIDEE-CAN (Naudts et al. 2015), CLM(SPA)
            (Bonan et al. 2014)
          - For details, see Supplements S9 and S10 in (Longo et al.
            2019b).
      - Multiple scatter model
          - Based on Zhao and Qualls (2005)
  - Crown model
      - Infinite (ED2 default)
          - Marginally taller cohorts completely shade out shorter ones
      - Finite
          - Cohorts have a finite crown area, so multiple cohorts have
            access to the same light level
          - Based on crown area allometries from Dietze and Clark (2008)
  - Trait plasticity
      - None (ED default) – Trait values are the same at all canopy
        heights and light levels
      - Plastic
          - V<sub>cmax</sub>(0) decreases with shading, and SLA
            increases with shading, based on Lloyd et al. (2010)
          - V<sub>cmax</sub>(0) decline with shade tolerance is
            supported by classic ecological theory on succession (Bazzaz
            1979)

## Data sources

BETYdb (LeBauer et al. 2013).

TRY data (Kattge et al. 2011).

CRUNCEP met.

Other FoRTE data? - LAI trends: 1997-2014 - Aboveground NPP data:
1999-2017

## Modeling workflow

PEcAn parameter sensitivity and uncertainty analysis (LeBauer et al.
2013). Other PEcAn references: (Dietze et al. 2013).

Plant functional type definitions:

  - **Early successional hardwood**
      - *Betula papyrifera* (white/paper birch)
      - *Populus grandidentata* (bigtooth aspen)
      - *Populus tremuloides* (trembling aspen)
  - **Mid successional hardwood**
      - *Quercus rubra* (red oak)
      - *Acer rubrum* (red maple)
      - *Acer pensylvaticum* (striped maple)
  - **Late successional hardwood**
      - *Acer saccharum* (sugar maple)
      - *Fagus grandifolia* (American beech)
  - **Pine**
      - *Pinus strobus* (white pine)
  - *Omitted candidates*
      - Early hardwood
          - *Prunus pensylvatica* (fire cherry)
      - Mid hardwood
          - *Betula alleghaniensis* (yellow birch)
          - *Tilia americana* (basswood)
          - *Fraxinus americana* (white ash)
      - Late hardwood
          - *Ostyra virginiana* (hop-hornbeam)
      - Pine
          - *Pinus resinosa* (red pine)

| ED Name                           | Description                                            | unit\_markdown                                         | Raczka                       | Dietze         | Ran |
| :-------------------------------- | :----------------------------------------------------- | :----------------------------------------------------- | :--------------------------- | :------------- | :-- |
| `mort1`                           | Time-scale at which low-carbon balance plants die      | years<sup>-1</sup>                                     | Carbon balance mortality     | Mortality      | no  |
| `root_turnover_rate`              | Temperature dependent rate of fine root loss           | year<sup>-1</sup>                                      | Root turnover                | Root turnover  | yes |
| `growth_resp_factor`              | Fraction of daily C gain lost to growth respiration    | unitless (0-1)                                         | Growth respiration           | Growth Resp    | yes |
| `stomatal_slope`                  | Slope between *A* and stomatal conductance (Leuning)   | unitless                                               | Stomatal sensitivity         | Stomatal Slope | yes |
| `fineroot2leaf (q)`               | Ratio of fine root to leaf biomass                     | unitless (mass ratio)                                  | Root/Leaf carbon             | Leaf:Root      | yes |
| `r_fract`                         | Fraction of C storage to seed reproduction             | unitless                                               | Recruitment carbon           | Reproduction?  | yes |
| `f_labile`                        | Fraction of litter that goes to labile (fast) C pool   | unitless (0-1)                                         | Labile carbon                | NA             | yes |
| `water_conductance`               | “Water availability factor”                            | m<sup>-2</sup> a<sup>-1</sup> (kg C root)<sup>-1</sup> | Soil-plant water conductance | Water Cond     | yes |
| `mort3`                           | Density-independent (background) mortality rate        | year<sup>-1</sup>                                      | Background mortality         | NA             | no  |
| `SLA`                             | Specific leaf area                                     | m<sup>2</sup> kg<sup>-1</sup> C                        | Specific leaf area           | SLA            | yes |
| `Rd0`                             | Leaf dark respiration at 15 °C                         | ???                                                    | Leaf respiration\*           | Leaf Resp\*    | yes |
| `dark_respiration_factor`         | Ratio of leaf respiration to Vcmax                     | ???                                                    | Leaf respiration\*           | Leaf Resp\*    | yes |
| `quantum_efficiency`              | Farquhar model parameter (TODO)                        | mol CO<sub>2</sub> (mol photons)<sup>-1</sup>          | Quantum efficiency           | Quantum Eff.   | yes |
| `Vcmax (Vm0)`                     | Maximum rate of CO<sub>2</sub> carboxylation at 15 °C  | μmol m<sup>-2</sup> s<sup>-1</sup>                     | Vcmax                        | Vcmax          | yes |
| `root_respiration_rate (_factor)` | Root respiration rate at 15 °C                         | μmol CO<sub>2</sub> (kg fineroot)<sup>-1</sup>         | Root respiration             | NA             | yes |
| `minimum_height`                  | Minimum height for plant reproduction                  | m                                                      | Minimum height               | NA             | no  |
| `leaf_reflect_vis`                | Leaf reflectance in visible range (400-700 nm)         | unitless (0-1)                                         | NA                           | NA             | no  |
| `leaf_reflect_nir`                | Leaf reflectance in NIR range (700-2500 nm)            | unitless (0-1)                                         | NA                           | NA             | no  |
| `leaf_trans_vis`                  | Leaf transmittance in visible range (400-700 nm)       | unitless (0-1)                                         | NA                           | NA             | no  |
| `leaf_trans_nir`                  | Leaf transmittance in NIR range (700-2500 nm)          | unitless (0-1)                                         | NA                           | NA             | no  |
| `c2n_fineroot`                    | C:N ratio in fine roots                                | unitless (mass ratio)                                  | NA                           | NA             | yes |
| `c2n_leaf`                        | C:N ratio in leaves                                    | unitless (mass ratio)                                  | NA                           | NA             | yes |
| `leaf_turnover_rate`              | Temperature dependent rate of leaf loss (conifer only) | year<sup>-1</sup>                                      | NA                           | NA             | yes |
| `leaf_width`                      | Mean leaf width (for boundary layer conductance)       | m                                                      | NA                           | NA             | yes |
| `mort2`                           | C balance ratio at which mortality rapidly increases   | unitless (mass ratio)                                  | NA                           | NA             | yes |
| `nonlocal_dispersal`              | “Proportion of dispersal that is global”               | unitless (0-1)                                         | NA                           | NA             | yes |
| `seedling_mortality`              | Proportion of seed that dies and goes to litter pool   | unitless (0-1)                                         | NA                           | NA             | yes |
| `Vm_low_temp`                     | Minimum temperature for photosynthesis                 | °C                                                     | NA                           | NA             | yes |

**Table**: Parameter descriptions. For additional information, see the
[ED2 model wiki](https://github.com/EDmodel/ED2/wiki/PFT-parameters).

![Input parameter distributions from PEcAn trait
meta-analysis.](paper_files/figure-gfm/param_dist-1.png)

**Figure 1**: Input parameter distributions from PEcAn trait
meta-analysis.

For each combination of ED submodels, we ran 10 ED ensemble simulations.

## Analysis of results

  - Structural vs. parameteric uncertainty
  - Comparison against observations
  - Others…

# Results

## Summary

![ED2 plot-level ensemble predictions of gross (GPP) and net (NPP)
primary productivity, total leaf area index (LAI), and aboveground
biomass (AGB) by model configuration. Red lines are mean (dashed) and
min/max (dotted) values from Hardiman et
al. (2013).](paper_files/figure-gfm/summary-ts-plot-1.png)

**Figure 2**: ED2 plot-level ensemble predictions of gross (GPP) and net
(NPP) primary productivity, total leaf area index (LAI), and aboveground
biomass (AGB) by model configuration. Red lines are mean (dashed) and
min/max (dotted) values from Hardiman et al. (2013).

Take-away: Parameter uncertainty is as large as, or even larger, than
structural uncertainty.

![ED2 PFT-level predictions of leaf area index (LAI) by model
configuration and
parameterization.](paper_files/figure-gfm/bigfacet-plot-1.png)

**Figure 3**: ED2 PFT-level predictions of leaf area index (LAI) by
model configuration and parameterization.

Take-away: Different parameter combinations produce not just
quantitatively different bulk ecosystem properties (e.g. NPP, total
LAI), but also qualitatively different successional/ecological outcomes.

## Uncertainty analysis

![Comparison of parameteric uncertainty within ensembles (colored bars,
colored by model type) and “structural” uncertainty (variance in
ensemble means; black bar) by output variable. Output variables are
expressed as growing season averages for all years after
1910.](paper_files/figure-gfm/within-vs-across-1.png)

**Figure 4**: Comparison of parameteric uncertainty within ensembles
(colored bars, colored by model type) and “structural” uncertainty
(variance in ensemble means; black bar) by output variable. Output
variables are expressed as growing season averages for all years after
1910.

Take-away: Overall, structural uncertainty (variance in ensemble means
across structures) is comparable or lower than parameter uncertainty
(uncertainty within ensembles for a given structure). Parameter
uncertainty varies significantly across model structures. Enabling trait
plasticity increases variance in predictions of AGB. Enabling the finite
canopy radius increases the variance in LAI predictions. GPP and NPP
have the highest variance by far for the combination of finite canopy
radius and two-stream radiative transfer.

![PEcAn-liked parameter sensitivity and uncertainty analysis, by model
type. Elasticity (a) is the normalized sensitivity of the model to a
fixed change in the parameter. pvar (b) is the partial variance, which
describes the overall contribution of the parameter to model predictive
uncertainty based on the combination of parameter uncertainty and model
sensitivity. Input parameter distributions are shown in Figure
XXX.](paper_files/figure-gfm/uncertainty-analysis-1.png)

**Figure 5**: PEcAn-liked parameter sensitivity and uncertainty
analysis, by model type. Elasticity (a) is the normalized sensitivity of
the model to a fixed change in the parameter. pvar (b) is the partial
variance, which describes the overall contribution of the parameter to
model predictive uncertainty based on the combination of parameter
uncertainty and model sensitivity. Input parameter distributions are
shown in Figure XXX.

Take-away: Model parameter uncertainty is sensitive to model
configuration. For example, turning on the finite canopy radius with the
two-stream canopy RTM increases sensitivity across many parameters.

## Details about model structures

**Figure 6**: Comparison of radiation profiles under two-stream and
multiple scatter canopy radiative transfer models.

<!-- Compared to the default two-stream model, the multiple scatter model leads to slightly less beam (direct) radiation in the understory but slightly more diffuse radiation. -->

(TODO)

# Discussion

  - Comparisons against other studies
      - Parameter sensitivity
          - (Pandit et al. 2018) – Most important parameters for shrubs
            were SLA, Vcmax, stomatal slope, root:leaf ratio, and root
            turonver rate (slightly less important: leaf turnover rate,
            growth respiration, cuticular conductance; much less
            important: water conductance; basically irrelevant: storage
            turnover, leaf width)
  - Ecological implications of structural uncertainty
      - Predictions about ecological succession are very sensitive to
        the understory light environment.
          - Need a good representation of canopy structure (finite
            canopy radius)
          - How you model canopy radiative transfer matters\! Small
            differences in the model can give significant results.
      - But, trait plasticity can provide an alternative mechanism for
        success in the understory.
          - Important modeling implication for all the emerging research
            on intra-specific trait variability.
          - This combination of discrete functional types with plastic
            traits is closer to reality than purely trait-based
            (“eco-evolutionary?”) models or completely discretized
            models with static traits on the other.
          - Important caveat: Current implementation is based entirely
            on tropical data, which has more extreme canopy shading than
            temperate forests (REF), so tropical species may have
            evolved stronger trait plasticity than temperate species
            (REF).
  - Implications of uncertainty analysis for future data collection and
    model development
      - Parameter uncertainty is still a *huge* source of overall model
        predictive uncertainty. Different parameters can give
        *qualitatively* different ecological outcomes.
      - Looking beyond aggregate variables (e.g. net primary
        productivity, total LAI) is essential to model evaluation and
        calibration. It’s possible to generate a relatively?
      - (Expected) Subsurface parameters (e.g. leaf-root ratios) have
        high sensitivity, but are poorly constrained.
      - Trait covariance may alleviate some data limitation (Shiklomanov
        et al. 2018 \[preprint\])
  - Future directions: Predicting disturbance response
      - FoRTE project – Forecasting a disturbance.
          - Compare ecological resilience/memory between model and
            reality

# Conclusions

<!-- TODO -->

# Acknowledgements

This project funded by NSF grant. Cyberinfrastructure provided by
Pacific Northwest National Laboratory (PNNL). Data from University of
Michigan Biological Station (UMBS). Data from TRY (TODO: Specific TRY
statement).

<!-- The following line inserts a page break when the output is MS Word. For page breaks in PDF, use \newpage on its own line.  -->

##### pagebreak

# References

<!-- The following line ensures the references appear here for the MS Word or HTML output files, rather than right at the end of the document (this will not work for PDF files):  -->

<div id="refs">

<div id="ref-bazzaz_1979_physiological">

Bazzaz, F. A. 1979. The physiological ecology of plant succession.
Annual Review of Ecology and Systematics 10:351–371.

</div>

<div id="ref-bergen_2007_observing">

Bergen, K. M., and I. Dronova. 2007. Observing succession on
aspen-dominated landscapes using a remote sensing-ecosystem approach.
Landscape Ecology 22:1395–1410.

</div>

<div id="ref-bonan_2014_modeling">

Bonan, G. B., M. Williams, R. A. Fisher, and K. W. Oleson. 2014.
Modeling stomatal conductance in the earth system: Linking leaf
water-use efficiency and water transport along the soil-plant-atmosphere
continuum. Geoscientific Model Development 7:2193–2222.

</div>

<div id="ref-dekauwe_2013_forest">

De Kauwe, M. G., B. E. Medlyn, S. Zaehle, A. P. Walker, M. C. Dietze, T.
Hickler, A. K. Jain, Y. Luo, W. J. Parton, I. C. Prentice, and et al.
2013. Forest water use and water use efficiency at elevated CO2: A
model-data intercomparison at two contrasting temperate forest FACE
sites. Global Change Biology 19:1759–1779.

</div>

<div id="ref-dietze_2008_changing">

Dietze, M. C., and J. S. Clark. 2008. Changing the gap dynamics
paradigm: Vegetative regeneration control on forest response to
disturbance. Ecological Monographs 78:331–347.

</div>

<div id="ref-dietze_2013_improving">

Dietze, M. C., D. S. LeBauer, and R. Kooper. 2013. On improving the
communication between models and data. Plant, Cell & Environment
36:1575–1585.

</div>

<div id="ref-dietze_2014_quantitative">

Dietze, M. C., S. P. Serbin, C. Davidson, A. R. Desai, X. Feng, R.
Kelly, R. Kooper, D. LeBauer, J. Mantooth, K. McHenry, and et al. 2014.
A quantitative assessment of a terrestrial biosphere model’s data needs
across North American biomes. Journal of Geophysical Research:
Biogeosciences 119:286–300.

</div>

<div id="ref-fisher_2017_vegetation">

Fisher, R. A., C. D. Koven, W. R. L. Anderegg, B. O. Christoffersen, M.
C. Dietze, C. E. Farrior, J. A. Holm, G. C. Hurtt, R. G. Knox, P. J.
Lawrence, J. W. Lichstein, M. Longo, A. M. Matheny, D. Medvigy, H. C.
Muller-Landau, T. L. Powell, S. P. Serbin, H. Sato, J. K. Shuman, B.
Smith, A. T. Trugman, T. Viskari, H. Verbeeck, E. Weng, C. Xu, X. Xu, T.
Zhang, and P. R. Moorcroft. 2017. Vegetation demographics in earth
system models: A review of progress and priorities. Global Change
Biology 24:35–54.

</div>

<div id="ref-friedlingstein_2006_climate">

Friedlingstein, P., P. Cox, R. Betts, L. Bopp, W. von Bloh, V. Brovkin,
P. Cadule, S. Doney, M. Eby, I. Fung, and et al. 2006. Climate-carbon
cycle feedback analysis: Results from the C4MIP model intercomparison.
Journal of Climate 19:3337–3353.

</div>

<div id="ref-friedlingstein_2014_uncertainties">

Friedlingstein, P., M. Meinshausen, V. K. Arora, C. D. Jones, A. Anav,
S. K. Liddicoat, and R. Knutti. 2014. Uncertainties in CMIP5 climate
projections due to carbon cycle feedbacks. Journal of Climate
27:511–526.

</div>

<div id="ref-friedman_2005_regional_legacies_logging">

Friedman, S. K., and P. B. Reich. 2005. Regional legacies of logging:
Departure from presettlement forest conditions in northern Minnesota.
Ecological Applications 15:726–744.

</div>

<div id="ref-gates_1930_aspen">

Gates, F. C. 1930. Aspen association in northern Lower Michigan.
Botanical Gazette 90:233–259.

</div>

<div id="ref-gough_2013_sustained">

Gough, C. M., B. S. Hardiman, L. E. Nave, G. Bohrer, K. D. Maurer, C. S.
Vogel, K. J. Nadelhoffer, and P. S. Curtis. 2013. Sustained carbon
uptake and storage following moderate disturbance in a great lakes
forest. Ecological Applications 23:1202–1215.

</div>

<div id="ref-gough_2008_multi">

Gough, C. M., C. S. Vogel, H. P. Schmid, H.-B. Su, and P. S. Curtis.
2008. Multi-year convergence of biometric and meteorological estimates
of forest carbon storage. Agricultural and Forest Meteorology
148:158–170.

</div>

<div id="ref-hurtt_2004_beyond_potential_vegetation">

Hurtt, G. C., R. Dubayah, J. Drake, P. R. Moorcroft, S. W. Pacala, J. B.
Blair, and M. G. Fearon. 2004. Beyond potential vegetation: Combining
lidar data and a height-structured model for carbon studies. Ecological
Applications 14:873–883.

</div>

<div id="ref-kattge_2011_try">

Kattge, J., S. Díaz, S. Lavorel, I. C. Prentice, P. Leadley, G. Bönisch,
E. Garnier, M. Westoby, P. B. Reich, I. J. Wright, J. H. C. Cornelissen,
C. Violle, S. P. Harrison, P. M. Van Bodegom, M. Reichstein, B. J.
Enquist, N. A. Soudzilovskaia, D. D. Ackerly, M. Anand, O. Atkin, M.
Bahn, T. R. Baker, D. Baldocchi, R. Bekker, C. C. Blanco, B. Blonder, W.
J. Bond, R. Bradstock, D. E. Bunker, F. Casanoves, J. Cavender-Bares, J.
Q. Chambers, F. S. Chapin III, J. Chave, D. Coomes, W. K. Cornwell, J.
M. Craine, B. H. Dobrin, L. Duarte, W. Durka, J. Elser, G. Esser, M.
Estiarte, W. F. Fagan, J. Fang, F. Fernández-Méndez, A. Fidelis, B.
Finegan, O. Flores, H. Ford, D. Frank, G. T. Freschet, N. M. Fyllas, R.
V. Gallagher, W. A. Green, A. G. Gutierrez, T. Hickler, S. I. Higgins,
J. G. Hodgson, A. Jalili, S. Jansen, C. A. Joly, A. J. Kerkhoff, D.
Kirkup, K. Kitajima, M. Kleyer, S. Klotz, J. M. H. Knops, K. Kramer, I.
Kühn, H. Kurokawa, D. Laughlin, T. D. Lee, M. Leishman, F. Lens, T.
Lenz, S. L. Lewis, J. Lloyd, J. Llusià, F. Louault, S. Ma, M. D.
Mahecha, P. Manning, T. Massad, B. E. Medlyn, J. Messier, A. T. Moles,
S. C. Müller, K. Nadrowski, S. Naeem, Ü. Niinemets, S. Nöllert, A.
Nüske, R. Ogaya, J. Oleksyn, V. G. Onipchenko, Y. Onoda, J. Ordoñez, G.
Overbeck, W. A. Ozinga, S. Patiño, S. Paula, J. G. Pausas, J. Peñuelas,
O. L. Phillips, V. Pillar, H. Poorter, L. Poorter, P. Poschlod, A.
Prinzing, R. Proulx, A. Rammig, S. Reinsch, B. Reu, L. Sack, B.
Salgado-Negret, J. Sardans, S. Shiodera, B. Shipley, A. Siefert, E.
Sosinski, J.-F. Soussana, E. Swaine, N. Swenson, K. Thompson, P.
Thornton, M. Waldram, E. Weiher, M. White, S. White, S. J. Wright, B.
Yguel, S. Zaehle, A. E. Zanne, and C. Wirth. 2011. TRY – a global
database of plant traits. Global Change Biology 17:2905–2935.

</div>

<div id="ref-kilburn_1960_effects">

Kilburn, P. D. 1960. Effects of logging and fire on xerophytic forests
in northern Michigan. Bulletin of the Torrey Botanical Club 87:402.

</div>

<div id="ref-lebauer_2013_facilitating">

LeBauer, D. S., D. Wang, K. T. Richter, C. C. Davidson, and M. C.
Dietze. 2013. Facilitating feedbacks between field measurements and
ecosystem models. Ecological Monographs 83:133–154.

</div>

<div id="ref-lloyd_2010_optimisation">

Lloyd, J., S. Patiño, R. Q. Paiva, G. B. Nardoto, C. A. Quesada, A. J.
B. Santos, T. R. Baker, W. A. Brand, I. Hilke, H. Gielmann, and et al.
2010. Optimisation of photosynthetic carbon gain and within-canopy
gradients of associated foliar traits for Amazon forest trees.
Biogeosciences 7:1833–1859.

</div>

<div id="ref-longo_2019_ed2">

Longo, M., R. G. Knox, N. M. Levine, A. L. S. Swann, D. M. Medvigy, M.
C. Dietze, Y. Kim, K. Zhang, D. Bonal, B. Burban, and et al. 2019a. The
biophysics, ecology, and biogeochemistry of functionally diverse,
vertically- and horizontally-heterogeneous ecosystems: The Ecosystem
Demography model, version 2.2 - part 2: Model evaluation. Geoscientific
Model Development Discussions:1–34.

</div>

<div id="ref-longo_2019_ed1">

Longo, M., R. G. Knox, D. M. Medvigy, N. M. Levine, M. C. Dietze, Y.
Kim, A. L. S. Swann, K. Zhang, C. R. Rollinson, R. L. Bras, and et al.
2019b. The biophysics, ecology, and biogeochemistry of functionally
diverse, vertically- and horizontally-heterogeneous ecosystems: The
Ecosystem Demography model, version 2.2 - part 1: Model description.
Geoscientific Model Development Discussions:1–53.

</div>

<div id="ref-medvigy_2011_predicting">

Medvigy, D., and P. R. Moorcroft. 2011. Predicting ecosystem dynamics at
regional scales: An evaluation of a terrestrial biosphere model for the
forests of northeastern North America. Philosophical Transactions of the
Royal Society B: Biological Sciences 367:222–235.

</div>

<div id="ref-medvigy_2009_mechanistic">

Medvigy, D., S. C. Wofsy, J. W. Munger, D. Y. Hollinger, and P. R.
Moorcroft. 2009. Mechanistic scaling of ecosystem function and dynamics
in space and time: Ecosystem demography model version 2. Journal of
Geophysical Research 114.

</div>

<div id="ref-mercado_2007_improving">

Mercado, L. M., C. Huntingford, J. H. C. Gash, P. M. Cox, and V.
Jogireddy. 2007. Improving the representation of radiation interception
and photosynthesis for climate model applications. Tellus B 59.

</div>

<div id="ref-miller_2015_alteration">

Miller, A. D., M. C. Dietze, E. H. DeLucia, and K. J. Anderson-Teixeira.
2015. Alteration of forest succession and carbon cycling under elevated
CO2. Global Change Biology 22:351–363.

</div>

<div id="ref-moorcroft_2001_method">

Moorcroft, P. R., G. C. Hurtt, and S. W. Pacala. 2001. A method for
scaling vegetation dynamics: The ecosystem demography model (ed).
Ecological Monographs 71:557–586.

</div>

<div id="ref-naudts_2015_vertically">

Naudts, K., J. Ryder, M. J. McGrath, J. Otto, Y. Chen, A. Valade, V.
Bellasen, G. Berhongaray, G. Bönisch, M. Campioli, and et al. 2015. A
vertically discretised canopy description for ORCHIDEE (SVN r2290) and
the modifications to the energy, water and carbon fluxes. Geoscientific
Model Development 8:2035–2065.

</div>

<div id="ref-clm45_note">

Oleson, K. W., D. M. Lawrence, G. B. Bonan, B. Drewniak, M. Huang, C. D.
Koven, S. Levis, F. Li, W. J. Riley, Z. M. Subin, S. C. Swenson, P.
Thornton, A. Bozbiyik, R. Fisher, C. L. Heald, E. Kluzek, J.-F.
Lamarque, P. J. Lawrence, R. Leung, W. Lipscom, S. Muszala, D. M.
Ricciuto, W. Sacks, Y. Sun, J. Tang, and Y. Zong-Liang. 2013. Technical
description of version 4.5 of the Community Land Model (CLM). NCAR Earth
System Laboratory Climate; Global Dynamics Division.

</div>

<div id="ref-pandit_2018_optimizing">

Pandit, K., H. Dashti, N. F. Glenn, A. N. Flores, K. C. Maguire, D. J.
Shinneman, G. N. Flerchinger, and A. W. Fellows. 2018. Optimizing shrub
parameters to estimate gross primary production of the sagebrush
ecosystem using the Ecosystem Demography (EDv2.2) model. Geoscientific
Model Development Discussions:1–23.

</div>

<div id="ref-purves_2008_predictive">

Purves, D., and S. Pacala. 2008. Predictive models of forest dynamics.
Science 320:1452–1453.

</div>

<div id="ref-raczka_2018_what">

Raczka, B., M. C. Dietze, S. P. Serbin, and K. J. Davis. 2018. What
limits predictive certainty of long‐term carbon uptake? Journal of
Geophysical Research: Biogeosciences 123:3570–3588.

</div>

<div id="ref-rogers_2016_roadmap">

Rogers, A., B. E. Medlyn, J. S. Dukes, G. Bonan, S. von Caemmerer, M. C.
Dietze, J. Kattge, A. D. B. Leakey, L. M. Mercado, Ü. Niinemets, and et
al. 2016. A roadmap for improving the representation of photosynthesis
in earth system models. New Phytologist 213:22–42.

</div>

<div id="ref-rollinson_2017_emergent">

Rollinson, C. R., Y. Liu, A. Raiho, D. J. P. Moore, J. McLachlan, D. A.
Bishop, A. Dye, J. H. Matthes, A. Hessl, T. Hickler, and et al. 2017.
Emergent climate and CO2 sensitivities of net primary productivity in
ecosystem models do not agree with empirical data in temperate forests
of eastern North America. Global Change Biology 23:2755–2767.

</div>

<div id="ref-shugart_2015_computer">

Shugart, H. H., G. P. Asner, R. Fischer, A. Huth, N. Knapp, T. Le Toan,
and J. K. Shuman. 2015. Computer and remote-sensing infrastructure to
enhance large-scale testing of individual-based forest models. Frontiers
in Ecology and the Environment 13:503–511.

</div>

<div id="ref-zhao_2005_multiple">

Zhao, W., and R. J. Qualls. 2005. A multiple-layer canopy scattering
model to simulate shortwave radiation distribution within a homogeneous
plant canopy. Water Resources Research 41.

</div>

</div>

# Appendix 1: Submodel descriptions

## Radiative transfer: Definitions

  - \(\omega\) – Clumping factor
  - \(LAI\) – Leaf area index
  - TAI – Total area index (leaf and woody area)
  - \(LAI_e\) / \(TAI_e\) – “Effective” area indices (accounting for
    clumping)

## Two-stream RTM

<!-- TODO -->

## Multiple scatter RTM

\[ LAI_e = \omega LAI \]

\[ TAI_e = LAI_e + WAI \]

\[ TAI_{l} = \frac{TAI_e}{CAI} \]

Leaf (\(w_l\)) and wood (\(w_w\)) weights:

\[ w_l = \frac{LAI_e}{TAI_e} \] \[ w_w = 1 - w_l \]

Projected area, based on coefficients \(\phi_1\) and \(\phi_2\).

\[ a_{proj} = \phi_1 + \phi_2 \mu \]

\[ \lambda = \frac{a_{proj}}{\mu} \]

##### pagebreak

### Colophon

This report was generated on 2019-06-06 09:24:36 using the following
computational environment and dependencies:

    #> ─ Session info ──────────────────────────────────────────────────────────
    #>  setting  value                       
    #>  version  R version 3.6.0 (2019-04-26)
    #>  os       macOS High Sierra 10.13.6   
    #>  system   x86_64, darwin17.7.0        
    #>  ui       unknown                     
    #>  language (EN)                        
    #>  collate  en_US.UTF-8                 
    #>  ctype    en_US.UTF-8                 
    #>  tz       America/New_York            
    #>  date     2019-06-06                  
    #> 
    #> ─ Packages ──────────────────────────────────────────────────────────────
    #>  ! package       * version    date       lib source        
    #>    assertthat      0.2.1      2019-03-21 [1] CRAN (R 3.6.0)
    #>    backports       1.1.4      2019-04-10 [1] CRAN (R 3.6.0)
    #>    base64url       1.4        2018-05-14 [1] CRAN (R 3.6.0)
    #>    bookdown        0.10       2019-05-10 [1] CRAN (R 3.6.0)
    #>    callr           3.2.0      2019-03-15 [1] CRAN (R 3.6.0)
    #>    cli             1.1.0      2019-03-19 [1] CRAN (R 3.6.0)
    #>    codetools       0.2-16     2018-12-24 [3] CRAN (R 3.6.0)
    #>    colorspace      1.4-1      2019-03-18 [1] CRAN (R 3.6.0)
    #>    cowplot       * 0.9.4      2019-01-08 [1] CRAN (R 3.6.0)
    #>    crayon          1.3.4      2017-09-16 [1] CRAN (R 3.6.0)
    #>    desc            1.2.0      2018-05-01 [1] CRAN (R 3.6.0)
    #>    devtools        2.0.2      2019-04-08 [1] CRAN (R 3.6.0)
    #>    digest          0.6.19     2019-05-20 [1] CRAN (R 3.6.0)
    #>    dplyr         * 0.8.1      2019-05-14 [1] CRAN (R 3.6.0)
    #>    drake         * 7.3.0      2019-05-19 [1] CRAN (R 3.6.0)
    #>    evaluate        0.14       2019-05-28 [1] CRAN (R 3.6.0)
    #>    forcats       * 0.4.0      2019-02-17 [1] CRAN (R 3.6.0)
    #>  P fortebaseline * 0.0.0.9000 2019-05-24 [?] local         
    #>    fs            * 1.3.1      2019-05-06 [1] CRAN (R 3.6.0)
    #>    fst           * 0.9.0      2019-04-09 [1] CRAN (R 3.6.0)
    #>    furrr         * 0.1.0      2018-05-16 [1] CRAN (R 3.6.0)
    #>    future        * 1.13.0     2019-05-08 [1] CRAN (R 3.6.0)
    #>    future.callr  * 0.4.0      2019-01-07 [1] CRAN (R 3.6.0)
    #>    ggplot2       * 3.1.1      2019-04-07 [1] CRAN (R 3.6.0)
    #>    ggsci           2.9        2018-05-14 [1] CRAN (R 3.6.0)
    #>    globals         0.12.4     2018-10-11 [1] CRAN (R 3.6.0)
    #>    glue            1.3.1      2019-03-12 [1] CRAN (R 3.6.0)
    #>    gtable          0.3.0      2019-03-25 [1] CRAN (R 3.6.0)
    #>    here          * 0.1        2017-05-28 [1] CRAN (R 3.6.0)
    #>    highr           0.8        2019-03-20 [1] CRAN (R 3.6.0)
    #>    hms             0.4.2      2018-03-10 [1] CRAN (R 3.6.0)
    #>    htmltools       0.3.6      2017-04-28 [1] CRAN (R 3.6.0)
    #>    igraph          1.2.4.1    2019-04-22 [1] CRAN (R 3.6.0)
    #>    knitr           1.23       2019-05-18 [1] CRAN (R 3.6.0)
    #>    labeling        0.3        2014-08-23 [1] CRAN (R 3.6.0)
    #>    lazyeval        0.2.2      2019-03-15 [1] CRAN (R 3.6.0)
    #>    listenv         0.7.0      2018-01-21 [1] CRAN (R 3.6.0)
    #>    lubridate     * 1.7.4      2018-04-11 [1] CRAN (R 3.6.0)
    #>    magrittr        1.5        2014-11-22 [1] CRAN (R 3.6.0)
    #>    memoise         1.1.0      2017-04-21 [1] CRAN (R 3.6.0)
    #>    munsell         0.5.0      2018-06-12 [1] CRAN (R 3.6.0)
    #>    ncdf4           1.16.1     2019-03-11 [1] CRAN (R 3.6.0)
    #>    pecanapi        1.7.0      2019-02-25 [1] local         
    #>    pillar          1.4.1      2019-05-28 [1] CRAN (R 3.6.0)
    #>    pkgbuild        1.0.3      2019-03-20 [1] CRAN (R 3.6.0)
    #>    pkgconfig       2.0.2      2018-08-16 [1] CRAN (R 3.6.0)
    #>    pkgload         1.0.2      2018-10-29 [1] CRAN (R 3.6.0)
    #>    plyr            1.8.4      2016-06-08 [1] CRAN (R 3.6.0)
    #>    prettyunits     1.0.2      2015-07-13 [1] CRAN (R 3.6.0)
    #>    processx        3.3.1      2019-05-08 [1] CRAN (R 3.6.0)
    #>    ps              1.3.0      2018-12-21 [1] CRAN (R 3.6.0)
    #>    purrr         * 0.3.2      2019-03-15 [1] CRAN (R 3.6.0)
    #>    R6              2.4.0      2019-02-14 [1] CRAN (R 3.6.0)
    #>    Rcpp            1.0.1      2019-03-17 [1] CRAN (R 3.6.0)
    #>    readr         * 1.3.1      2018-12-21 [1] CRAN (R 3.6.0)
    #>    remotes         2.0.4      2019-04-10 [1] CRAN (R 3.6.0)
    #>    reshape2        1.4.3      2017-12-11 [1] CRAN (R 3.6.0)
    #>    rlang           0.3.4      2019-04-07 [1] CRAN (R 3.6.0)
    #>    rmarkdown       1.13       2019-05-22 [1] CRAN (R 3.6.0)
    #>    rprojroot       1.3-2      2018-01-03 [1] CRAN (R 3.6.0)
    #>    rstudioapi      0.10       2019-03-19 [1] CRAN (R 3.6.0)
    #>    scales          1.0.0      2018-08-09 [1] CRAN (R 3.6.0)
    #>    sessioninfo     1.1.1      2018-11-05 [1] CRAN (R 3.6.0)
    #>    storr           1.2.1      2018-10-18 [1] CRAN (R 3.6.0)
    #>    stringi         1.4.3      2019-03-12 [1] CRAN (R 3.6.0)
    #>    stringr         1.4.0      2019-02-10 [1] CRAN (R 3.6.0)
    #>    testthat        2.1.1      2019-04-23 [1] CRAN (R 3.6.0)
    #>    tibble          2.1.1      2019-03-16 [1] CRAN (R 3.6.0)
    #>    tidyr         * 0.8.3      2019-03-01 [1] CRAN (R 3.6.0)
    #>    tidyselect      0.2.5      2018-10-11 [1] CRAN (R 3.6.0)
    #>    usethis         1.5.0      2019-04-07 [1] CRAN (R 3.6.0)
    #>    withr           2.1.2      2018-03-15 [1] CRAN (R 3.6.0)
    #>    xfun            0.7        2019-05-14 [1] CRAN (R 3.6.0)
    #>    xml2            1.2.0      2018-01-24 [1] CRAN (R 3.6.0)
    #>    yaml            2.2.0      2018-07-25 [1] CRAN (R 3.6.0)
    #> 
    #> [1] /Users/shik544/R
    #> [2] /usr/local/lib/R/3.6/site-library
    #> [3] /usr/local/Cellar/r/3.6.0_2/lib/R/library
    #> 
    #>  P ── Loaded and on-disk path mismatch.

The current Git commit details are:

    #> Local:    master /Users/shik544/Box Sync/Projects/forte_project/fortebaseline
    #> Remote:   master @ origin (git@github.com:ashiklom/fortebaseline.git)
    #> Head:     [e60da00] 2019-06-06: Render ED2 list in intro correctly
