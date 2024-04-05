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
        ':</div><div class="custom-body"><span class="label label-danger">Compacteur</span>' +
        '<br><br><span class="label label-info">Outre Mer</span>' +
        '<br><br><span class="label label-primary">Site classique</span>',
    placement: 'bottom',
    trigger: 'hover',
    html: true
});

    }






      Shiny.addCustomMessageHandler("updatePopSite", function(message) {
          var i = message.i;
          var region = message.region;
          var maisonMere = message.maisonMere;
          var tonnage = message.tonnage;
          var rotation = message.rotation;
          var compacteur = message.compacteur;

          // Mettre à jour le contenu du popover existant
          updatePopSite(i, region, maisonMere, tonnage, rotation, compacteur);
      });
      function updatePopSite(i, region, maisonMere, tonnage, rotation, compacteur) {
      var content = '<div class="custom-header">Information site ' + i +
          ':</div><div class="custom-body"><b>Region: </b>' + region +
          '<br><b>Maison mère: </b>' + maisonMere +
          '<br><b>Tonnage: </b>' + tonnage +
          '<br><b>Nombre de rotation: </b>' + rotation +
          '<br><b>Compacteur: </b>' + compacteur + '</div>';

      $('#cadre' + i + '-site1').attr('data-content', content);

      var placement = (i === 5) ? 'top' : 'bottom';

      // Mettre à jour le contenu du popover et spécifier les options trigger et html
      $('#cadre' + i + '-site1').popover({
          content: content,
          placement: placement,
          trigger: 'hover', // Afficher le popover au survol
          html: true // Le contenu du popover contient du HTML
      });
  }


});
