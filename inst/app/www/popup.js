$(document).ready(function() {
    Shiny.addCustomMessageHandler("initializePopover", function(message) {
        var i = message.i;
        var IC = message.IC;
        var minSite = message.minSite;
        var minTonnage = message.minTonnage;
        var maxSite = message.maxSite;
        var maxTonnage = message.maxTonnage;
        var nbSites = message.nbSites;
        var nbOutreMer = message.nbOutreMer;
        var nbCompacteur = message.nbCompacteur;

        initializePopover(i, IC, minSite, minTonnage, maxSite, maxTonnage, nbSites, nbOutreMer, nbCompacteur);
    });

    function initializePopover(i, IC, minSite, minTonnage, maxSite, maxTonnage, nbSites, nbOutreMer, nbCompacteur) {
        $('#info_icon' + i).popover({
            content: '<div class="custom-header">Information classe ' + i +
                ':</div><div class="custom-body"><b>Intervalle de tonnage: </b>' + IC +
                '<br><b>MinTonnage: </b>' + minSite + ':' + minTonnage +
                '<br><b>MaxTonnage: </b>' + maxSite + ':' + maxTonnage +
                '<br><b>Nombre de sites: </b>' + nbSites +
                '<br><b>Dont outre mers: </b>' + nbOutreMer +
                '<br><b>Dont compacteur: </b>' + nbCompacteur + '</div>',
            placement: 'right',
            trigger: 'hover',
            html: true
        });
    }
});
