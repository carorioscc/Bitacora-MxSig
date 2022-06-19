$.widget("custom.downloadlist", {
  //--------Codigo de Widget-------------------------------------------------------------------------------

  // default options
  options: {
	  map:null,
	  urlService:'',
	  title:''
  },

  // The constructor
  _create: function () {
    var obj = this;
    obj.id = obj.element.attr('id');
	obj.map = obj.options.map;
    this.element
      // add a class for theming
      .addClass("downloadlist-container onmapchanged").attr('widgetname','downloadlist');

    this._refresh();
	if(!$('#main').attr('initalized'+obj.id)){
		$('#main').attr('initalized'+obj.id,true);	
		
		obj.options.showDialog(obj.options.message);
	}
	  
	  
	  
	obj.download();
  },

  // Called when created, and later when changing options
  _refresh: function () {
    // Trigger a callback/event
    this._trigger("change");
  },


  // Events bound via _on are removed automatically
  // revert other modifications here
  _destroy: function () {
    // remove generated elements
    this.changer.remove();

    this.element
      .removeClass("custom-downloadlist")
      .enableSelection()
      .css("background-color", "transparent");
  },

  // _setOptions is called with a hash of all options that are changing
  // always refresh when changing options
  _setOptions: function () {
    // _super and _superApply handle keeping the right this-context
    this._superApply(arguments);
    this._refresh();
  },

  // _setOption is called for each individual option that is changing
  _setOption: function (key, value) {
    // prevent invalid color values
    this._super(key, value);
  },
	
  // Custom *******************************************************************************************
  mapChanged:function(){
	var obj = this;
	obj.download();
  },
  download:function(){
	var obj = this;
	var extent = obj.map.getExtent();
	var x1 = extent.lat[0],
		y1 = extent.lat[1],
		x2 = extent.lon[0],
		y2 = extent.lon[1];

	var polygon = 'POLYGON(('+x1+' '+y1+','+x2+' '+y1+','+x2+' '+y2+','+x1+' '+y2+','+x1+' '+y1+'))';

	//obtencion de datos
	var getData = function(func){
		var centroid = obj.map.getDistanceFromCentroid().centroid;
		var centroid = obj.map.transformToMercator(centroid.lon,centroid.lat);



		var mainObj = {
			resolution:obj.map.getResolution(),
			centroid:'POINT('+centroid.lon+' '+centroid.lat+')',
			polygon:polygon
		};

		mainObj = JSON.stringify(mainObj);
		var params = mainObj;
		
		var connection = {
			url:obj.options.urlService,
			contentType : "application/json; charset=utf-8",
			type: 'POST',
			dataType: "json",
			data:mainObj
			
		}

		var dataSource = $.extend(true,{},connection);
		$.ajax(dataSource).done(function(data) {
			if(data && data.response.success){
			   if($.isFunction(func))
					func(data);
			}
		}).error(function(){
			obj.map.Notification({message:'La funcionalidad de descargas no esta disponible, favor de intentarlo m&aacute;s tarde',time:4500});
		});

	}

	getData(function(data){
		obj.createDownloadUI(data);
	});

  },
  createDownloadUI:function(data){
	var obj = this;
	  
	if(data){
		var list = data.data.list;

			var edos = data.edos;
			var muns = data.muns;
			var locs = data.locs;


			var cadena = '';//'<div id="downloadlist_download" class="downloadlist-container">';
				cadena+= '	<div class="downloadlist-header"><div class="downloadlist-title">'+obj.options.title+'</div><div id="downloadlist-close" class="downloadlist-sprite downloadlist-sprite-close"></div></div>';
				cadena+= '	<hr />';
				cadena+= '  <label class="downloadlist-type">'+data.data.type+'</label>';
				cadena+= '	<div class="downloadlist-list">';

				if(list.length > 0){
					var type = (data.data.type == 'localidad')?'loc':
								(data.data.type == 'municipal')?'mun':
								(data.data.type == 'estatal')?'edo':'nac';

					for(var x in list){
						var item = list[x];
						cadena+= '<div class="downloadlist-list-item" idref="'+item.cvegeo+'">';	
						cadena+= '	<div class="downloadlist-list-icon downloadlist-sprite downloadlist-sprite-'+type+'"></div>';	
						cadena+= '	<div class="downloadlist-list-text">'+item.name+'</div>';	
						cadena+= '	<div class="downloadlist-list-icon-download downloadlist-sprite downloadlist-sprite-download"></div>';
						cadena+= '</div>';	
					}

				}else{
					cadena+= '<label class="downloadlist-notFound">No se encontraron elementos en la vista actual</label>';	
				}

				cadena+= '	</div>';
				/*if(obj.map.getResolution() <= 76.437028271){
					cadena+= '	<div class="downloadlist-bottom-tools">';
					cadena+= '		<div id="downloadlist-view" title="Descargar los datos de la vista actual"><div class="downloadlist-sprite downloadlist-sprite-viewDownload"></div><label>Descargar vista actual</label></div>';
					cadena+= '	</div>';
				}*/

				cadena+= ''//'</div>';
			
			$('#'+obj.id).html('');
			$('#'+obj.id).html(cadena);
			$('#downloadlist-close').click(function(){
				$('#'+obj.id).remove();	
			})
			$('.downloadlist-list-item').each(function(){
				$(this).click(function(){
					var session = obj.map.idSession;
					var gid = $(this).attr('idref');
					var url = obj.options.downloadURL.replace('%s',gid);
					window.open(url);
				});
			});
			/*$('#downloadlist-view').click(function(){
				var extent = obj.map.getExtent();
				var x1 = extent.lat[0],
					y1 = extent.lat[1],
					x2 = extent.lon[0],
					y2 = extent.lon[1];

				var pg1 = obj.map.transformToGeographic(x1,y1);
				var pg2 = obj.map.transformToGeographic(x2,y2);

				var polygon = 'POLYGON(('+pg1.lon+' '+pg1.lat+','+pg2.lon+' '+pg1.lat+','+pg2.lon+' '+pg2.lat+','+pg1.lon+' '+pg2.lat+','+pg1.lon+' '+pg1.lat+'))';
				var session = obj.map.idSession;
				var url = obj.options.downloadURL;
				window.open(url+'?idSesion='+session+'&poligonoRecorte='+polygon);
			});*/
	}

}
});

