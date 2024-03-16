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


    Shiny.addCustomMessageHandler("initPopColor", function(message) {
        initPopColor();
    });

    function initPopColor() {
        $('#info_icon').popover({
            content: '<div class="custom-header">Code couleur' +
                ':</div><div class="custom-body"><span class="label label-danger"><i class="fa-solid fa-dumpster fa-beat"></i>Compacteur</span>' +
                '<br><br><span class="label label-info">Outre Mer</span>' +
                '<br><br><span class="label label-primary">Site classique</span>',
            placement: 'bottom',
            trigger: 'hover',
            html: true
        });
    }
});
