/* Copyright (c) 2006-2013 by OpenLayers Contributors (see authors.txt for
 * full list of contributors). Published under the 2-clause BSD license.
 * See license.txt in the OpenLayers distribution or repository for the
 * full text of the license. */


/**
 * @requires OpenLayers/Layer/Markers.js
 * @requires OpenLayers/Request/XMLHttpRequest.js
 */

/**
 * Class: OpenLayers.Layer.GeoRSS
 * Add GeoRSS Point features to your map. 
 * 
 * Inherits from:
 *  - <OpenLayers.Layer.Markers>
 */
OpenLayers.Layer.GeoRSS = OpenLayers.Class(OpenLayers.Layer.Markers, {

    /** 
     * Property: location 
     * {String} store url of text file 
     */
    location: null,

    /** 
     * Property: features 
     * {Array(<OpenLayers.Feature>)} 
     */
    features: null,
    
    /**
     * APIProperty: formatOptions
     * {Object} Hash of options which should be passed to the format when it is
     * created. Must be passed in the constructor.
     */
    formatOptions: null, 

    /** 
     * Property: selectedFeature 
     * {<OpenLayers.Feature>} 
     */
    selectedFeature: null,

    /** 
     * APIProperty: icon 
     * {<OpenLayers.Icon>}. This determines the Icon to be used on the map
     * for this GeoRSS layer.
     */
    icon: null,

    /**
     * APIProperty: popupSize
     * {<OpenLayers.Size>} This determines the size of GeoRSS popups. If 
     * not provided, defaults to 250px by 120px. 
     */
    popupSize: null, 
    
    /** 
     * APIProperty: useFeedTitle 
     * {Boolean} Set layer.name to the first <title> element in the feed. Default is true. 
     */
    useFeedTitle: true,
    
    /**
    * Constructor: OpenLayers.Layer.GeoRSS
    * Create a GeoRSS Layer.
    *
    * Parameters:
    * name - {String} 
    * location - {String} 
    * options - {Object}
    */
    initialize: function(name, location, options) {
        OpenLayers.Layer.Markers.prototype.initialize.apply(this, [name, options]);
        this.location = location;
        this.features = [];
    },

    /**
     * Method: destroy 
     */
    destroy: function() {
        // Warning: Layer.Markers.destroy() must be called prior to calling
        // clearFeatures() here, otherwise we leak memory. Indeed, if
        // Layer.Markers.destroy() is called after clearFeatures(), it won't be
        // able to remove the marker image elements from the layer's div since
        // the markers will have been destroyed by clearFeatures().
        OpenLayers.Layer.Markers.prototype.destroy.apply(this, arguments);
        this.clearFeatures();
        this.features = null;
    },

    /**
     * Method: loadRSS
     * Start the load of the RSS data. Don't do this when we first add the layer,
     * since we may not be visible at any point, and it would therefore be a waste.
     */
    loadRSS: function() {
        if (!this.loaded) {
            this.events.triggerEvent("loadstart");
            OpenLayers.Request.GET({
                url: this.location,
                success: this.parseData,
                scope: this
            });
            this.loaded = true;
        }    
    },    
    
    /**
     * Method: moveTo
     * If layer is visible and RSS has not been loaded, load RSS. 
     * 
     * Parameters:
     * bounds - {Object} 
     * zoomChanged - {Object} 
     * minor - {Object} 
     */
    moveTo:function(bounds, zoomChanged, minor) {
        OpenLayers.Layer.Markers.prototype.moveTo.apply(this, arguments);
        if(this.visibility && !this.loaded){
            this.loadRSS();
        }
    },
        
    /**
     * Method: parseData
     * Parse the data returned from the Events call.
     *
     * Parameters:
     * ajaxRequest - {<OpenLayers.Request.XMLHttpRequest>} 
     */
    parseData: function(ajaxRequest) {
        var thereisDataMex = false;
        var activeTrap = false;
        var typeFormat = 1; 
       
        var doc = ajaxRequest.responseXML;
        //var doc = OpenLayers.Format.GeoJSON.prototype.read(ajaxRequest.responseText);
        
        if (!doc || !doc.documentElement) {
            doc = OpenLayers.Format.XML.prototype.read(ajaxRequest.responseText);
            //doc = OpenLayers.Format.GeoJSON.prototype.read(ajaxRequest.responseText);
        }
        
        if (this.useFeedTitle) {
            var name = null;
            try {
                name = doc.getElementsByTagNameNS('*', 'title')[0].firstChild.nodeValue;
            }
            catch (e) {
                try{
                    name = doc.getElementsByTagName('title')[0].firstChild.nodeValue;
                }
                catch(e){
                    try{
                        //alert("leyendo geojson")
                        doc = OpenLayers.Format.GeoJSON.prototype.read(ajaxRequest.responseText);
                        if(doc==null){
                            GeoRss.showError();
                            activeTrap = true;
                        }else{
                            typeFormat=2;
                            doc = ajaxRequest.responseText;
                        }
                    }
                    catch(e){
                         GeoRss.showError();
                         activeTrap = true;
                    }
                    
                }
            }
            if (name) {
                this.setName(name);
            }    
        }
        var options = {};
        
        OpenLayers.Util.extend(options, this.formatOptions);
        
        if (this.map && !this.projection.equals(this.map.getProjectionObject())) {
            options.externalProjection = this.projection;
            options.internalProjection = this.map.getProjectionObject();
        }
        //console.log(typeFormat)
        var format = (typeFormat==1)?new OpenLayers.Format.GeoRSS(options):new OpenLayers.Format.GeoJSON(options);
        //var format = new OpenLayers.Format.GeoRSS(options);
        /////var format = new OpenLayers.Format.GeoJSON(options);
        //alert("leyeendo caracteriticas");
        if(!activeTrap){
            var source = GeoRss.getSource(this.options.pos);
            var features = format.read(doc);//ajaxRequest.responseText
            //console.log(features)
	    
            var exitPoint = false;
            for (var i=0, len=features.length; i<len; i++) {
                exitPoint = false;
                var data = {};
                var feature = features[i];
                // we don't support features with no geometry in the GeoRSS
                // layer at this time. 
                if (!feature.geometry) {
                    continue;
                }    
                
                var title = feature.attributes.title ? feature.attributes.title : "Untitled";
                
                var description = feature.attributes.description ? 
                             feature.attributes.description : "No description.";
                             
                var link = feature.attributes.link ? feature.attributes.link : "";
                
                if(typeFormat==2){
                    //title = source.title;
                    title = feature.attributes.place ? 
                             "M "+ feature.attributes.mag + " "+ feature.attributes.place : "Untitled";
                    description = feature.attributes.time ? new Date(feature.attributes.time): "No description.";
                    link = feature.attributes.url ? 
                             feature.attributes.url : "";
                }
                var location = feature.geometry.getBounds().getCenterLonLat();
                
                
                data.icon = this.icon == null ? 
                                         OpenLayers.Marker.defaultIcon() : 
                                         this.icon.clone();
                
                data.popupSize = this.popupSize ? 
                                 this.popupSize.clone() :
                                 new OpenLayers.Size(250, 200);
                
                if (title || description) {
                    // we have supplemental data, store them.
                    data.title = title;
                    data.description = description;
                
                    var contentHTML = '';//'<div class="olLayerGeoRSSClose">[x]</div>'; 
                    contentHTML += '<div class="olLayerGeoRSSTitle">';
                    //var source = GeoRss.getSource(this.options.pos);//
                    if (link) {
                        contentHTML += '<div><center>'+source.title+'</center></div><center>'+'<a class="link modificado_25" href="'+link+'" target="_blank">';//added
                    }
                    contentHTML += title;
                    if (link) {
                        contentHTML += '</a></center>';
                    }
                    contentHTML += '</div>';
                    contentHTML += '<div style="" class="modificado_20">';//class="olLayerGeoRSSDescription">'
                    contentHTML += GeoRss.modifyImg(description);
                    contentHTML += '</br>'+source.path;//added
                    contentHTML += '</div>';
                   
                    //data['popupContentHTML'] = contentHTML;
                    ///////////////////////////////////////////
                    data['popupContentHTML'] = GeoRss.getContent(contentHTML);
                    ////////////////////////////////////////////
                }
                //if(!thereisDataMex){
                
                    if((location.lon>=extentMapa.lon[0])&&(location.lon<=extentMapa.lat[0])){
                        if((location.lat>=extentMapa.lon[1])&&(location.lat<=extentMapa.lat[1])){
                                thereisDataMex = true;
                                exitPoint = true;
                                //console.log("true --"+location);
                                
                                GeoRss.reg[this.options.idGeoRss].items.push({title:title,des:description,coords:location,url:link});
                        }
                    }
                //}
                if(exitPoint){
                var feature = new OpenLayers.Feature(this, location, data);
                this.features.push(feature);
                var marker = feature.createMarker();
                marker.events.register('click', feature, this.markerClick);
                this.addMarker(marker);
                }
            }
        }
        if(!activeTrap){
            GeoRss.showError(thereisDataMex);
        }
        oculta_infoLoader();
        var item = this.options.idGeoRss.replace("Georss","cmpRss");
        var ilabel = this.options.idGeoRss.replace("Georss","lgrss");
        $("#"+item).removeAttr('process');
        $("#"+ilabel).next().remove();
        ilabel = ilabel.replace("l","i");
        $("#"+ilabel).removeAttr('disabled');
        if(GeoRss.first){
            $("#cmpRss1").click();
            GeoRss.first=false;
        }
        this.events.triggerEvent("loadend");
    },
    
    /**
     * Method: markerClick
     *
     * Parameters:
     * evt - {Event} 
     */
    markerClick: function(evt) {
        var sameMarkerClicked = (this == this.layer.selectedFeature);
        this.layer.selectedFeature = (!sameMarkerClicked) ? this : null;
        for(var i=0, len=this.layer.map.popups.length; i<len; i++) {
            this.layer.map.removePopup(this.layer.map.popups[i]);
        }
        if (!sameMarkerClicked) {
            var popup = this.createPopup();
            OpenLayers.Event.observe(popup.div, "click",
                OpenLayers.Function.bind(function() { 
                    for(var i=0, len=this.layer.map.popups.length; i<len; i++) { 
                        this.layer.map.removePopup(this.layer.map.popups[i]); 
                    }
                }, this)
            );
            this.layer.map.addPopup(popup); 
        }
        OpenLayers.Event.stop(evt);
    },

    /**
     * Method: clearFeatures
     * Destroy all features in this layer.
     */
    clearFeatures: function() {
        if (this.features != null) {
            while(this.features.length > 0) {
                var feature = this.features[0];
                OpenLayers.Util.removeItem(this.features, feature);
                feature.destroy();
            }
        }        
    },
    
    CLASS_NAME: "OpenLayers.Layer.GeoRSS"
});
