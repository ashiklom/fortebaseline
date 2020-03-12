import HDF5
import Dates
import Gadfly
import JLD

# directory = ARGS[1]
directory = "analysis/data/retrieved/oaat-sensitivity/water_conductance/Early hardwood/q025"

allfiles = readdir(directory)
monthfiles = allfiles[map(i -> occursin(r"analysis-E", i), allfiles)]

datestrings = map(x -> match(r"[[:digit:]]{4}-[[:digit:]]{2}", x).match, monthfiles)
dates = map(x -> Dates.Date(x, Dates.DateFormat("y-m")), datestrings)

function read_var(file, variable, dir = directory)
    return HDF5.h5read(dir * "/" * file, variable)
end

fullpath = directory * "/" * monthfiles[1]

function read_all(file, dir = directory)
    fullpath = dir * "/" * file
    hf = HDF5.h5open(fullpath)
    vars = HDF5.names(hf)
    values = map(x -> HDF5.read(hf, x), vars)
    HDF5.close(hf)
    return vars, values
end

all_data = map(read_all, monthfiles)

JLD.save("test-jld.jld", "all_data", all_data)

pft = map(x -> read_var(x, "PFT"), monthfiles)
ncohort = map(length, pft)
max_ncohort = findmax(ncohort)[1]

agb = map(x -> read_var(x, "AGB_CO"), monthfiles)
lai = map(x -> read_var(x, "LAI_CO"), monthfiles)

npp = collect(Iterators.flatten(map(read_npp, monthfiles)))

Gadfly.plot(x = dates, y = agb)
