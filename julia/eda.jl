using DataFrames
using CSV

export_rows = DataFrame(CSV.File("../exportations_activity_rows.csv"));
export_cols = DataFrame(CSV.File("../exportations_activity_cols.csv"));

names(export_cols)
## Names before translating
categorias = names(export_cols)[3:27];

activities_en = [#"state", "year",
                 "Total", "Food", "Drinks and tobacco",
                   "Textiles", "Textile products", "Tailoring",
                   "Paper", "Chemistry", "Plastic",
                 "Minerals based", "Metal industry",
                 "Metal products",
                 "Machinery", "Electronics",
                 "Transport equipment",
                 "Furniture", "Other manufactures",
                 "Not specified",
                   "Mining", "Leather", "Wood",
                   "Printing", "Electricity", "Petroleum",
                 "Petroleum products"]

## To rename specific columns we need a for loop
## Either traditional
for i in 3:27
    rename!(export_cols,
            (names(export_cols)[i] => activities_en[i-2]));
end

## Or list comprehension
rename!(export_cols,[
    (names(export_cols)[i] => activities_en[i-2]) for i in 3:27
]);
names(export_cols)

## Changing values in rows

function translate(vector_es)
    vector_en = []
    for i in 1:length(vector_es)
        expression_es = vector_es[i]
        expression_en = equivalent(expression_es)
        ## Here "equivalent" should take expression.es and
        ## return the equivalent in English
        vector_en = append!(vector_en, expression_en)
    end
    return(vector_en)
end


function equivalent(expression_es)
    position = findall(x -> x == expression_es, categorias)
    expression_en = activities_en[position]
    return(expression_en)
end

equivalent("Industria química")

translate(categorias)

## Creating a new column "Activity" with the English expressions

export_rows["Activity"] = translate(export_rows["Descripción"])

## We could also use the equivalent of R's mutate using the
## packages "DataFramesMeta"
using DataFramesMeta

export_rows = @linq export_rows |>
    transform(Act = translate(:"Descripción"));

## DataFramesMeta comes handy for several data munging techniques
totals = @linq export_cols |>
    by(:state, total_export = sum(:Total)) |>
    sort!(:total_export)

## First Plots

using Gadfly
set_default_plot_size(30cm, 20cm)

plot(totals, x = "state", y = "total_export", Geom.bar)

using VegaLite

totals |>
    @vlplot(:bar,
            y = {"state", sort = "-x"},
            x = {"total_export", 
                 axis={title="Total Exports"}})

export_cols |>
    @vlplot(:bar,
            y={"sum(Total)"},
            x={"state",
               sort = {encoding = "y"}})
