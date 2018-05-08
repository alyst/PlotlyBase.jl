"""
Given the number of rows and columns, return an NTuple{4,Float64} containing
`(width, height, hspace, vspace)`, where `width` and `height` are the
width and height of each subplot and `vspace` and `hspace` are the vertical
and horizonal spacing between subplots, respectively.
"""
function subplot_size(nr::Int, nc::Int, subplot_titles::Bool=false)
    # NOTE: the logic of this function was mostly borrowed from plotly.py
    dx = 0.2 / nc
    dy = subplot_titles ? 0.55 / nr : 0.3 / nr
    width = (1. - dx * (nc - 1)) / nc
    height = (1. - dy * (nr - 1)) / nr
    vspace = nr == 1 ? 0.0 : (1 - height*nr)/(nr-1)
    hspace = nc == 1 ? 0.0 : (1 - width*nc)/(nc-1)
    width, height, hspace, vspace
end

function grid_layout(widths::AbstractVector{<:Number}, heights::AbstractVector{<:Number},
                     horgaps::AbstractVector{<:Number}, vertgaps::AbstractVector{<:Number},
                     nplots::Integer = length(widths)*length(heights))
    nc = length(widths)
    nr = length(heights)
    length(horgaps) == nc+1 || throw(ArgumentError("Number of horizontal gaps does not match"))
    length(vertgaps) == nr+1 || throw(ArgumentError("Number of vertical gaps does not match"))
    (all(x -> x > 0.0, widths) && all(x -> x > 0.0, heights) &&
     all(x -> x >= 0.0, horgaps) && all(x -> x >= 0.0, vertgaps)) || throw(ArgumentError("Negative lengths"))

    hscale = 1.0/(sum(widths)+sum(horgaps))
    vscale = 1.0/(sum(heights)+sum(vertgaps))

    out = Layout()

    x = horgaps[1]*hscale  # start from left
    subplot = 1
    for col in eachindex(widths)
        (subplot > nplots) && break
        w = widths[col]*hscale
        y = 1.0-vertgaps[1]*vscale  # start from top
        for row in eachindex(heights)
            h = heights[row]*vscale

            out["xaxis$subplot"] = Dict{Any,Any}(:domain=>[x, x+w], :anchor=>"y$subplot")
            out["yaxis$subplot"] = Dict{Any,Any}(:domain=>[y-h, y], :anchor=>"x$subplot")

            y -= (h + vertgaps[row+1]*vscale)
            subplot += 1
            (subplot > nplots) && break
        end

        x += w + horgaps[col+1]*hscale
    end
    out
end

function subplots_layout(nr, nc, subplot_titles::Bool=false)
    w, h, dx, dy = subplot_size(nr, nc, subplot_titles)

    out = Layout()

    x = 0.0  # start from left
    for col in 1:nc

        y = 1.0 # start from top
        for row in 1:nr
            subplot = LinearIndices((nr, nc))[row, col]

            out["xaxis$subplot"] = Dict{Any,Any}(:domain=>[x, x+w], :anchor=> "y$subplot")
            out["yaxis$subplot"] = Dict{Any,Any}(:domain=>[y-h, y], :anchor=> "x$subplot")

            y -= h + dy
        end

        x += w + dx
    end

    return out
end

hastitle(layout::Layout) = haskey(layout.fields, "title") || haskey(layout.fields, :title)
hastitle(plot::Plot) = hastitle(plot.layout)

function add_subplot_annotation!(big_layout::Layout, sub_layout::Layout, ix::Integer)
    hastitle(sub_layout) || return big_layout

    # check for symbol or string
    subtitle = pop!(sub_layout.fields, haskey(sub_layout.fields, "title") ? "title" : :title)

    # add text annotation with the subplot title
    ann = Dict{Any,Any}(:font => Dict{Any,Any}(:size => 16),
                        :showarrow => false,
                        :text => subtitle,
                        :x => mean(big_layout["xaxis$(ix).domain"]),
                        :xanchor => "center",
                        :xref => "paper",
                        :y => big_layout["yaxis$(ix).domain"][2],
                        :yanchor => "bottom",
                        :yref => "paper")
    anns = get(big_layout.fields, :annotations, Dict{Any,Any}[])
    push!(anns, ann)
    big_layout[:annotations] = anns
    big_layout
end

# plots are 3d if any of their traces have a 3d type. This should flow down
# the methods as ordered here
_is3d(p::Plot) = any(_is3d, p.data)
_is3d(t::GenericTrace) = _is3d(t[:type])
_is3d(t::Symbol) = _is3d(string(t))
_is3d(s::AbstractString) = s in ["surface", "mesh3d", "scatter3d"]

# else (maybe if trace didn't have a :type field set)
_is3d(x::Any)= false

function arrange(layout::Layout, subplots::AbstractVector{<:Plot})
    copied_subplots = Plot[copy(p) for p in subplots]

    for (ix, plot) in enumerate(copied_subplots)
        add_subplot_annotation!(layout, plot.layout, ix)
        layout["xaxis$ix"] = merge(plot.layout["xaxis"], layout["xaxis$ix"])
        layout["yaxis$ix"] = merge(plot.layout["yaxis"], layout["yaxis$ix"])

        if _is3d(plot)
            # need to move (x|y)axis$ix into scene$ix here
            layout["scene$ix"] = attr(
                xaxis=pop!(layout, "xaxis$(ix)"),
                yaxis=pop!(layout, "yaxis$(ix)")
            )
            for trace in plot.data
                trace["scene"] = "scene$ix"
            end
        else
            for trace in plot.data
                trace["xaxis"] = "x$ix"
                trace["yaxis"] = "y$ix"
            end
        end
    end

    Plot(vcat([p.data for p in copied_subplots]...), layout)
end

arrange(layout::Layout, subplots::Plot...) = arrange(layout, [subplots...])

arrange(subplots::AbstractVector{<:Plot}) =
    arrange(subplots_layout(length(subplots), 1, any(hastitle, subplots)), vec(subplots))
arrange(subplots::AbstractMatrix{<:Plot}) =
    arrange(subplots_layout(size(subplots)..., any(hastitle, subplots)), vec(subplots))
