$ ->
    interfaces = {} 
    interface_count = 0
    barChart = {}
    barWidth = 0
    data = []
    virus_set = []
    exclude_set = []
    association = false

    color = d3.interpolateRgb("#aad", "#556")

    get_interfaces = (viruses) ->
        interfaces = {}
        interface_count = 0

        _.map viruses, (virus, v_index) ->
            v_data = virus.interfaces
            virus_set.push virus.entry_id
            min_value = _.chain(v_data).pluck('assocn_nrg_total').min().value() * 0.05
            v_interfaces = _.filter v_data, (interface) ->
                interface.assocn_nrg_total < min_value
            _.each v_interfaces, (i, index) ->
                i.name = "#{i.auth_1_asym_id}#{i.viper_matrix_1}-#{i.auth_2_asym_id}#{i.viper_matrix_2}"
                if not interfaces[i.name]?
                    interfaces[i.name] = []
                    interface_count++
                interfaces[i.name].push
                    entry_id: virus.entry_id
                    association: i.assocn_nrg_total
                    buried: i.bsa_total

    set_interface_data = () ->
        data = []
        i = 0
        for own interface, matched_viruses of interfaces
            data[i] = []
            for virus in matched_viruses
                if virus.entry_id not in exclude_set
                    data[i].push 
                        interface: interface
                        virus: virus.entry_id
                        value: if association then virus.association else virus.buried
            i++

    get_virus_info = (family, exclude=[]) ->
        $.ajax
            url: "http://localhost:8000/api/v1/virus/" 
            data: 
                format: 'json'
                family: "Leviviridae"
            success: (virus_data) ->
                get_interfaces(virus_data.objects)
                set_interface_data()
                update_legend()
                update()
            error: (e) ->
                console.log e
            dataType: 'json'

    get_virus_color = (virus) ->
        index = _.indexOf virus_set, virus
        color(index / (virus_set.length - 1))

    width = 960
    barHeight = 15 
    height = 400
    legendWidth = 100

    x = d3.scale.linear()
    y = d3.scale.linear()

    barChart = d3.select("#barChart")
                .append('svg')
                .attr('width', width)
                .attr('transform', "translate(10, 15)")

    legend = d3.select("#chartLegend")
            .append('svg')
            .attr('width', legendWidth)

    update = () ->
        viruses = _.difference virus_set, exclude_set
        unique_entries = viruses.length * interface_count


        # This is where we change the domain, ranges, and scales
        if association
            x.domain([d3.min(data, (datum) ->
                d3.min(datum, (data) -> data.value)), 0])
            x.rangeRound([-(width-50), 0])
        else
            x.domain([0, d3.max(data, (datum) -> 
                d3.max(datum, (data) -> data.value))])
            x.rangeRound([0, (width-50)])
        y.domain([0, unique_entries])

        # Changing height of chart to reflect entries
        height = ((barHeight)+5) * unique_entries
        barChart.transition()
            .delay((d, i) -> return i * 10)
            .attr('height', height)
        y.range([0, height-(barHeight)])

        # Setting up the axes
        yAxis = barChart.selectAll('text.yAxis').data(data)
        yAxis.enter().append('text')
            .attr('x', 0)
            .attr("class", "yAxis")
            .attr("fill", "black")
            .attr("style", "font-size: 12; font-family: Helvetica, sans-serif")
            .text((datum) -> datum[0].interface)
        yAxis.attr('y', (datum, index, group) -> y(index * viruses.length))
             .attr('dy',((barHeight+10) * viruses.length)/2) 
        yAxis.exit().remove()

        interface_layer = barChart.selectAll('g.interface').data(data)
        interface_layer.enter()
            .append('g').attr('class', 'interface')
        interface_layer.exit().remove()

        bars = interface_layer.selectAll('rect').data((datum) -> datum)
        bars.enter()
            .append('rect')
                .attr('width', 0)
                .attr('height', barHeight)
                .attr('class', 'bar') 
        bars.attr('y', (datum, index, group) ->
                index = _.indexOf viruses, datum.virus
                y((group * viruses.length) + (index*.9))
            )
            .attr('fill', (datum) -> get_virus_color datum.virus)
        bars.transition()
            .delay((d, i) -> i * 10)
            .attr('width', (datum, index, group) ->
                Math.abs(x(datum.value))
            )
            .attr('x', (datum) ->
                if association then width - Math.abs(x(datum.value)) else 40
            )
        bars.exit().remove();

        bar_labels = interface_layer.selectAll('text').data((datum) -> datum)
        bar_labels.enter().append('text')
                .attr("dx", 5)
                .attr("dy", (barHeight + 5) /2)
                .attr("text-anchor", "right")
                .attr("style", "font-size: 10; font-family: Helvetica, sans-serif")
                .attr('fill', 'white')
        bar_labels.transition()
            .delay((d, i) -> i * 10)
            .attr('x', (datum, index, group) -> 
                if association then width - Math.abs(x(datum.value)) else Math.abs(x(datum.value))
            )
            .attr("text-anchor", (datum) -> if association then "left" else "right")
        bar_labels.attr("y", (datum, index, group) ->
                index = _.indexOf viruses, datum.virus
                y((group * viruses.length) + (index*.9))
            )
            .text((datum)-> datum.virus)
        bar_labels.exit().remove()

        
    update_legend = () ->
        viruses = virus_set
        boxWidth = 20
        boxHeight = 20
        height = 30 * viruses.length

        legend.attr('height', height)

        y = d3.scale.linear().domain([0, viruses.length]).range([0, height])

        legend_layer = legend.selectAll("rect").data(viruses)

        legend_layer.enter().append('rect')
            .attr('y', (datum, index) -> y(_.indexOf viruses, datum))
            .attr('fill', (datum) -> get_virus_color datum, viruses)
            .attr('height', boxHeight)
            .attr('width', boxWidth)

        legend_layer.exit().remove()

        legend_text = legend.selectAll('text.legend').data(viruses)

        legend_text.enter().append('svg:text')
            .attr('x', boxWidth + 10) 
            .attr('y', (datum, index) -> y(_.indexOf viruses, datum) + boxHeight)
            .attr("style", "font-size: 12; font-family: Helvetica, sans-serif")
            .attr("fill", "black")
            .attr("class", "yAxis")
            .attr('dy', -boxHeight / 4)
            .text((datum) -> datum)

        legend_text.exit().remove()

        $("#chartLegend text").on 'click', (e) ->
            clicked_virus = $(e.currentTarget).text()
            exclude_index = _.indexOf(exclude_set, clicked_virus)
            if exclude_index == -1
                exclude_set.push clicked_virus
            else
                exclude_set.splice exclude_index, 1
            set_interface_data()
            update()

    get_virus_info('Leviviridae')

    $("#js-graphtype").on 'click', (e) ->
        association = not association
        set_interface_data()
        update()






