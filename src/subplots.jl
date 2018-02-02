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

function handle_titles!(big_layout, sub_layout, ix::Int)
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

function _cat(nr::Int, nc::Int, ps::Plot...)
    copied_plots = Plot[copy(p) for p in ps]
    subplot_titles = any(hastitle, ps)
    layout = subplots_layout(nr, nc, subplot_titles)

    for col in 1:nc, row in 1:nr
        ix = LinearIndices((nc, nr))[col, row]
        handle_titles!(layout, copied_plots[ix].layout, ix)
        layout["xaxis$ix"] = merge(copied_plots[ix].layout["xaxis"], layout["xaxis$ix"])
        layout["yaxis$ix"] = merge(copied_plots[ix].layout["yaxis"], layout["yaxis$ix"])

        if _is3d(copied_plots[ix])
            # need to move (x|y)axis$ix into scene$ix here
            layout["scene$ix"] = attr(
                xaxis=pop!(layout, "xaxis$(ix)"),
                yaxis=pop!(layout, "yaxis$(ix)")
            )
            for trace in copied_plots[ix].data
                trace["scene"] = "scene$ix"
            end
        else
            for trace in copied_plots[ix].data
                trace["xaxis"] = "x$ix"
                trace["yaxis"] = "y$ix"
            end
        end

    end

    Plot(vcat([p.data for p in copied_plots]...), layout)
end

Base.hcat(ps::Plot...) = _cat(1, length(ps), ps...)
Base.vcat(ps::Plot...) = _cat(length(ps), 1,  ps...)
Base.vect(ps::Plot...) = vcat(ps...)

function Base.hvcat(rows::Tuple{Vararg{Int}}, ps::Plot...)
    nr = length(rows)
    nc = rows[1]

    for (i, c) in enumerate(rows[2:end])
        c == nc || error("Saw $c columns in row $(i+1), expected $nc")
    end
    _cat(nr, nc, ps...)
end
