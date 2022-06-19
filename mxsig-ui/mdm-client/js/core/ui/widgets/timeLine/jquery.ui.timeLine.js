//Limitado a 10 elementos por capa
//solo despliega capas que se encuentran en el listado de capas proporcionado
$.widget( "custom.timeLine", {
      id:'',
	  timer:0,
	  firstRun:true,
	  isRunning:false,
	  errorOnLoad:false,
	  repeat:false,
	  mainIndex:0,
	  countLoad:0,
	  mainOpacity:1,
	  mainDelay:60,
	  progress:0,
	  ancho:42,
	  activeLayers:[],
	  timelayers:{},
	  sizeElements:0,
	  options:{
		  urlSlider:null,//'http://10.152.11.6/fcgi-bin/ms62/mapserv.exe?map=/opt/map/mdm60/mdm6vectorjusticia.map&FORMAT=image%2Fpng&MAXRESOLUTION=4891.969809375&MINZOOMLEVEL=5&ZOOMOFFSET=5&TATO=0&LAYERNAME=&SERVICE=WMS&VERSION=1.1.1&REQUEST=GetMap&STYLES=&FIRM=383&SRS=EPSG%3A900913&BBOX=-11489015.995879,2459972.9977607,-11293337.203505,2533046.7967883&WIDTH=1280&HEIGHT=478&LAYERS=',
		  layersList:{},
		  url:'',
		  timeItemSize:35,
		  externalActivate:function(name){},
		  externalDeactivate:function(name){},
		  internalDeactivate:function(layer){},
		  onTimePlayerOpen:function(current){},
		  onTimePlayerClose:function(current){},
		  onTimeout:function(){},
		  getCurrentTransparency:function(){}
      },
	  //Control de slider---------------------------------------------------------------------------------
	  timePlay:function(idLayer){
		  var obj= this;
		  var capas = obj.activeLayers;
		  var capa = null;
		  for(var x in capas){
			  if(idLayer == capas[x].id)
			  	capa = capas[x];
		  }
		  //var capa = obj.activeLayers[0];
		  
		  var years = capa.years;
		  var url = obj.options.urlSlider;
		  var image=$('.timeline-years-selector').attr('idref');
		  var pos=$('.timeline-years-selector').attr('position');
		  
		  obj.isRunning = true;
		  
		  var listYears =[];
		  //listado de años
		  for(var x in years){
			  listYears.push(x);
		  }
		  
		  //Ocultar todas las imagenes 
		  $('.timeline-image').each(function(index, element) {
                $(this).css('display','none');
          });
		  
		  var index = obj.mainIndex;
		  var maxpos = listYears.length-1;
		 
		  var animate = function(){ 
			  			if(obj.isRunning){ 
						  if (obj.mainIndex != maxpos){
							
							 obj.isRunning = true;
							 var currentImage = $('#tl_img_'+listYears[obj.mainIndex]);
							 var nextImage = $('#tl_img_'+listYears[obj.mainIndex+1]);
							 
							 //Si es el comienzo de la opacidad
							 if(obj.mainOpacity == 1){
								 if(obj.mainIndex == 0){
									 for(var x in listYears){
										$('#tl_img_'+listYears[x]).css('display','none');
									}
								 }
								 for(var x = 0; x <=obj.mainIndex; x++){
									$('#timeline-years'+listYears[x]+' .timeline-progress').css('width','100%');	 
								 } 
								 for(var x in listYears){
									if(x > obj.mainIndex)
										$('#timeline-years'+listYears[x]+' .timeline-progress').css('width','0%');
								 }
								 $('#timeline_title').html(listYears[obj.mainIndex]);
							 }
							 var currentYear = $('#idref_'+listYears[obj.mainIndex]+' .progress');
							 
							 currentImage.css('display','block');
							 nextImage.css('display','block');
							 
							 currentImage.css('opacity',obj.mainOpacity);
							 //nextImage.css('opacity',(1-obj.mainOpacity));
							 
							 var avancePorcentil = (25/100)*((1-obj.mainOpacity)*100);
							 var progressPos = 73+((obj.mainIndex*25)+avancePorcentil)
							 $('#timeline_progressBar').css('left',progressPos+'px');
						 
							 if(obj.mainOpacity > 0){
									obj.mainOpacity = obj.mainOpacity-0.01;
									
									setTimeout(function(){
										animate();
									},obj.mainDelay);
									
							 }else{
								
							  currentImage.css('opacity','').css('display','none');
							
								if(obj.mainIndex != maxpos){
									obj.mainIndex  = obj.mainIndex+1;	 
									obj.mainOpacity = 1; 
									
									setTimeout(function(){
											animate();
								    },obj.mainDelay);
								}
							 }
						  }else{
							 
							 $('#timeline_title').html(listYears[obj.mainIndex]);
							 //$('#tl_img_'+listYears[obj.mainIndex]).css('display','block');
							 obj.isRunning = true;
							 obj.mainIndex = 0;
							 obj.mainOpacity = 1;
							 
							 if (obj.repeat){
								 setTimeout(function(){
									animate();
								 },2500);
							 }else{
								 //ajusta control de reproduccion
								$('#timeline_player_container').attr('mode','pause'); 
							 }
						  }
						}
			  }
		  
		  animate();
		  
	},
	openPlayer:function(id){
		var obj= this;
		//timeout para carga de imagenes
		obj.timer = setTimeout(function(){
			obj.options.onTimeout('El tiempo de carga de las im&aacute;genes fue excedido');
			obj.hideSpinner();
			obj.closePlayer(id);	
		},60000);
		obj.hide(function(){
			obj.initTimeSlider(id);	
			obj.options.onTimePlayerOpen(id);
		});
		
		
		/*
		obj.timer = setTimeout(function(){
			obj.options.onTimeout('El tiempo de carga de las im&aacute;genes fue excedido');
			obj.hideSpinner();
			obj.closePlayer(id);	
		},60000);
		
		obj.hide(function(){
			obj.options.onTimePlayerOpen(id);
			obj.errorOnLoad = false;
			obj.preloadMapImages(id,
								//success
								function(){
									clearTimeout(obj.timer);
									obj.initTimeSlider(id);
									setTimeout(function(){
										//$('#timeline_playBtn').click();
									},700);
								},
								//Error
								function(){
									if(!obj.errorOnLoad){
										clearTimeout(obj.timer);
										obj.closePlayer(id);
										obj.options.onTimeout('Error al intentar cargar las imagenes de la linea de tiempo');
										obj.errorOnLoad = true;
									}
								});
			
			var timeLayer = obj.getActiveLayer(id);
			if (timeLayer){
				var treeLayer = $.extend({},obj.getLayer(id));
				treeLayer.idLayer = timeLayer;
				//obj.options.externalDeactivate(treeLayer);		
			}
			
		});*/
	},
	displayPlayer:function(type){
		if(type){
			$('#timeline_player').css('display','block');
		}else{
			$('#timeline_player').css('display','none');
		}
	},
	closePlayer:function(id){
		var obj= this;
		//if($('#timeline_player').attr('idref')){
		
			 obj.options.onTimePlayerClose(id);
			 $('.timeline-player').remove();
			 
			  obj.mainIndex=0;
	  		  obj.mainOpacity=1;
			  obj.isRunning=false;
			  
			  var timeLayer = obj.getActiveLayer(id);
			  if (timeLayer){
					var treeLayer = $.extend({},obj.getLayer(id));
					treeLayer.idLayer = timeLayer;
					//obj.options.externalActivate(treeLayer);		
			 }
			  
			 obj.show();
		//}
	},
	preloadMapImages:function(idLayer,func,error){
		  var obj= this;
		  obj.currentLayer = idLayer;
		  var capas = obj.activeLayers;
		  var actLayers = obj.getActiveLayers();
		  var capa = null;
		  var map = obj.options.map;
		  for(var x in capas){
			  if(idLayer == capas[x].id)
			  	capa = capas[x];
		  }
		  
		  var years = capa.years;
		  var url = obj.options.urlSlider;
		  //definicion de parametros de imagen
		  var urlParams = map.getWmsParams();
		  var strParams = ''
		  for(var x in urlParams){
			  if(x != 'LAYERS')
			  	strParams+= '&'+x+'='+urlParams[x];
		  }
		  strParams+='&LAYERS=';
		  url+=strParams;
		
		  //----------------------------------------
		  var listImages = [];
		  for(var x in years){
			  var ids =   actLayers.replace(idLayer,years[x]);
			  //listImages.push(url+years[x]);
			  listImages.push(url+ids);
		  }
		  var cadena= '<div id="timeline_preloadSpinner" class="timeline-preloadSpinner-bg"><div id="timeline_preloadSpinner" class="timeline-preloadSpinner"><label id="timeline_preloadSpinner_progress" style="position:absolute;top:17px;left:10px;font-size:165%;width:40px;text-align:center"></label></div></div>';
		  $('body').append(cadena);
		  //preload images
		  obj.preloadPictures(listImages,
		  		//callback
				function(){
					if($.isFunction(func)){
						obj.hideSpinner();
						func();	
					}	
		  		},
				//progress
				function(text){
					   $('#timeline_preloadSpinner_progress').html(text);
						
		  		},
				//error
				function(){
					if($.isFunction(error)){
						obj.hideSpinner();
						error();	
					}	
		  		});
	},
	showSpinner:function(text){
		var obj = this;
		if(!$('#timeline_preloadSpinner').attr('id')){
			var cadena= '<div id="timeline_preloadSpinner" class="timeline-preloadSpinner-bg"><div id="timeline_preloadSpinner" class="timeline-preloadSpinner"><label id="timeline_preloadSpinner_progress" style="position:absolute;top:17px;left:10px;font-size:165%;width:40px;text-align:center"></label></div></div>';
			$('body').append(cadena);
		}
		if(text){
			$('#timeline_preloadSpinner_progress').html(text);
		}
	},
	hideSpinner:function(){
		$('#timeline_preloadSpinner').fadeOut('slow',function(){
			$('#timeline_preloadSpinner').remove();
		});
	},
	initTimeSlider:function(idLayer){
		  var obj= this;
		
		  obj.showSpinner();
		
		  obj.currentLayer = idLayer;
		  var transparency = obj.options.getCurrentTransparency();
		  var actLayers = obj.getActiveLayers();
		  var capas = obj.activeLayers;
		  var capa = null;
		  var map = obj.options.map;
		  for(var x in capas){
			  if(idLayer == capas[x].id)
			  	capa = capas[x];
		  }
		  var years = capa.years;
		  var url = obj.options.urlSlider;
		  var cadena2 = '';
		  var listYears =[];
	
		  //definicion de parametros de imagen
		  var urlParams = map.getWmsParams();
		  var strParams = ''
		  for(var x in urlParams){
			  if(x != 'LAYERS')
			  	strParams+= '&'+x+'='+urlParams[x];
		  }
		  strParams+='&LAYERS=';
		  url+=strParams;
		  //----------------------------------------
		  var z_index = 50;
		  
		  var urlLayers = [];
		  for(var x in years){
			  urlLayers.push(  actLayers.replace(idLayer,years[x])  );
		  }
	      var cont = 0;
		
		  for(var x in years){
			  listYears.push(x);
			  cadena2+='<img id="tl_img_'+x+'" src="'+url+urlLayers[cont]+'" class="timeline-image" style="position:absolute;z-index:'+z_index+';left:0px;top:0px;'+((listYears.length > 1)?'display:none;':'')+'"/>';
		  	  z_index--;
			  cont++;
		  }
		
	     var cadena = '<div id="timeline_player" class="timeline-player" idref="'+idLayer+'" style="display:none">';
			 cadena+= ' <div class="timeline-imgs-container" style="opacity:'+transparency+'">'+cadena2+'</div>';
		     cadena+= ' <div id="timeline_title" class="timeline-title"></div>';
		     cadena+= '	<div id="timeline_title_layer" class="timeline-title-layer">'+capa.properties.label+'</div>'; 
			 cadena+= '	<div id="timeline_closeBtn" class="timeline-closeBtn timeLine-sprite timeLine_close"></div>'; 
			  
			  
			 //New Player
			 cadena+= '<div id="timeline_player_container" class="timeline-player-container" speed="x1" mode="play" style="display:none">';
			 cadena+= '		<div class="timeline-speedControl">';
			 cadena+= '			<div id="timeline_1x_bg"></div>';
			 cadena+= '			<div id="timeline_2x_bg"></div>';
			 cadena+= '			<div id="timeline_1x" class="timeLine-sprite timeLine_speed_1x"></div>';
			 cadena+= '			<div id="timeline_2x" class="timeLine-sprite timeLine_speed_2x"></div>';
			 cadena+= '		</div>';
			 cadena+= '		<div class="timeline-controlButtons">';
			 cadena+= '			<div id="timeline_playBtn" idref="'+idLayer+'" class="timeline-playBtn timeLine-sprite timeLine_big_play"></div>';
			 cadena+= '			<div id="timeline_pauseBtn" class="timeline-pauseBtn timeLine-sprite timeLine_big_pause"></div>';
			 cadena+= '		</div>';
			 cadena+= '		<div id="timeline_progressBar" class="timeline-progressBar"></div>';
			 cadena+= '		<div class="timeline-yearsContainer" style="width:'+((Math.ceil(listYears.length/2)*50)+30)+'px">';
			 cadena+= '			<div class="timeline-yearsItems">';
			 
			 var upperContainer = '<div class="timeline-upperItems">';
			 var lowerContainer = '<div class="timeline-lowerItems">';;
			 
			 var upper = true;
			 for(var x in listYears){
				  var items = '<div id="timeline-item-'+listYears[x]+'" position="'+x+'" idref="tl_img_'+listYears[x]+'" year="'+listYears[x]+'" class="timeline-year-item '+((upper)?'upper-item':'lower-item')+' transition-fast">';
					  items+=  (upper)?'':'<div class="timeline-item-pillar"></div>';
					  items+= 	'<div class="timeline-item-label">'+listYears[x]+'</div>';
					  items+=  (upper)?'<div class="timeline-item-pillar"></div>':'';
					  items+= '</div>';
					  
				  if(upper)
				     upperContainer+=items
				  else
					 lowerContainer+=items;
					  
				  upper = !upper;
			  }
			 
			 upperContainer+= '</div>';
			 lowerContainer+= '</div>';
			 
			 cadena+= upperContainer;
			 cadena+= '<div class="timeline-years-bar"></div>';
			 cadena+= lowerContainer;
			 cadena+= '			</div>'; 
			 cadena+= '		</div>';
			 cadena+= '</div>';
			 
			 cadena+= '</div>';//main
			
			 $('#panel-center').append(cadena);
		
			 var checkImageIsReady = function(){
				 var ready = true;
				 var count = 0;
				 var trueCount = 0;
				 
				 $('.timeline-image').each(function(){
					count++;
					var loaded = $(this).attr('loaded');
					if(!loaded){
						ready = false;
					}else{
						trueCount++;
					}
				 });
				 
				 var percent = ((100/count)*trueCount).round();
				 obj.showSpinner(percent);
				 if(count == trueCount){
					 obj.hideSpinner();
				 }
				 
				 return ready;
			 }
			
		     $('.timeline-image').each(function(){
				$(this).load(function(){
					$(this).attr('loaded',true);
					if (checkImageIsReady()){
						clearTimeout(obj.timer);
						obj.displayPlayer(true);
						$('#timeline_playBtn').click();
						obj.hideSpinner();
					}
				}).error(function(){
					clearTimeout(obj.timer);
					var idref =  $('#timeline_player').attr('idref');
				  	obj.closePlayer(idref);
					obj.options.onTimeout('Error al intentar cargar las imagenes de la linea de tiempo');
					obj.errorOnLoad = true;
					obj.hideSpinner();
				});
			 });
		
			 
			 $('#timeline_1x').click(function(){
				$('#timeline_player_container').attr('speed','x1'); 
				obj.mainDelay = 60;
			 });
			 $('#timeline_2x').click(function(){
				$('#timeline_player_container').attr('speed','x2'); 
				obj.mainDelay = 10;
			 });
			 
			 $('#timeline_player_container').fadeIn('slow');
			 
			 $('#timeline_closeBtn').click(function(){
				  var idref =  $('#timeline_player').attr('idref');
				  obj.closePlayer(idref);
			 });
			 
			 $('#timeline_playBtn').click(function(){
				 $('#timeline_player_container').attr('mode','play');
				 var layer = $(this).attr('idref');
				 obj.timePlay(layer); 
			 });
			 $('#timeline_pauseBtn').click(function(){
				 $('#timeline_player_container').attr('mode','pause');
				  if(obj.isRunning){
					obj.isRunning = false;
				 }
			 });
			 $('.timeline-year-item').each(function(index, element) {
                $(this).click(function(){
					obj.isRunning= false;
					var image=$(this).attr('idref');
					var pos=parseInt($(this).attr('position'),10);
					var year=parseInt($(this).attr('year'),10);
					obj.mainIndex = pos;
					obj.mainOpacity = 1;
					
					$('.timeline-image').each(function(index, element) {
						$(this).css({'display':'none',
									  'opacity':''});
					});
					//ajuste de progreso
					var progressPos = 73+((obj.mainIndex*25));
					$('#timeline_progressBar').css('left',progressPos+'px');
					
					$("#"+image).css("display",'');
					//ajusta control de reproduccion
					$('#timeline_player_container').attr('mode','pause');
					//Asigna Titulo
					$('#timeline_title').html(listYears[obj.mainIndex]);
					
				});
            });
			 
	},

//fin slider-----------------------------------------------------------------------------------------
	  show:function(){
		  var obj = this;
		  obj.element.css('display','');
		  var height = obj.element.attr('height');
		  obj.element.animate({'height':height+'px'});
	  },
	  hide:function(func){
		  var obj = this;
		  obj.element.animate({'height':'0px'},function(){
				obj.element.css('display','none');
				if($.isFunction(func))
					func();    
		  });
	  },
	  activateLayer:function(name){
		var obj = this;
		var oldActive = obj.getActiveLayer(name);
		obj.setTreeLayer(name,true);
		obj.update();
		var timeLayer = obj.getActiveLayer(name);
		if (timeLayer){
			var treeLayer = $.extend({},obj.getLayer(name));
			treeLayer.idLayer = timeLayer;
			obj.externalActivate(treeLayer);
		}
	  },
	  deactivateLayer:function(name){
		var obj = this;
		var timeLayer = obj.getActiveLayer(name);
		if (timeLayer){
			obj.deleteActiveLayer(name);
			var treeLayer = $.extend({},obj.getLayer(name));
			treeLayer.idLayer = timeLayer;
			obj.setTreeLayer(name,false);
			obj.update();
			obj.externalDeactivate(treeLayer);
		}
	  },
	  resetActiveStatus:function(){
		var obj = this;
		obj.activeLayers = [];
		
		var groups = obj.options.layersList.groups;
		for(var x in groups){
			var group = groups[x];
			var layers = group.layers;
			for (var y in layers){
				var layer = layers[y];
				layer.active = false;
			}
		}
		
		
		obj.update();  
	  },
	  externalActivate:function(name){
		var obj=this;
		obj.options.externalActivate(name);
	  },
	  externalDeactivate:function(name){
		var obj=this;
		obj.options.externalDeactivate(name);
		obj.init();
	  },
	  internalDeactivate:function(id){
		var obj = this;
		var layer =  obj.getLayer(id);
		layer.idLayer = id;
		obj.options.internalDeactivate(layer);
	  },
	  convertlayer:function(name){
		var obj = this;
		var layers = obj.activeLayers;
		var result = name;
		for(var x in layers){
			if(layers[x].id == name){		
				result = layers[x].active;
				break;
			}
		}
		return result;
	  },
	  update:function(){
		var obj = this;
		obj.createStructure();  
	  },
	  deleteActiveLayer:function(name){
		var obj = this;
		var layers = obj.activeLayers;
		var r = null;
		for (var x in layers){
			if (layers[x].id == name){
				layers.splice(x,1);
				break;
			}
		}
		return r;  
	  },
	  getTimeLayer:function(name){
		var obj = this;
		var layers = obj.timeLayers;
		var r = null;
		for (var x in layers){
			if (name == x){
				r = layers[x].active;
				break;
			}
		}
		return r;  
	  },
	  getActiveLayers:function(name){
		var obj = this;
		var layers = obj.options.getActiveLayers();;
		var r = null;
		var list = {}
		var list2 = [];
		for (var x in layers){
			list['l_'+layers[x].position] = layers[x].idLayer;
		}
		for(var x in list){
			list2.push(list[x])
		}
		return list2.join();  
	  },
	  getActiveLayer:function(name){
		var obj = this;
		var layers = obj.activeLayers;
		var r = null;
		for (var x in layers){
			if (name == layers[x].id){
				r = layers[x].active;
				break;
			}
		}
		return r;  
	  },
	  setTreeLayer:function(name,value){
		var obj = this;
		var groups = obj.options.layersList.groups;
		var result = null;
		for(var x in groups){
			var group = groups[x];
			var layers = group.layers;
			for (var y in layers){
				var layer = layers[y];
				if (y == name){
					var isTime = layer.time;
					if(isTime){
						layer.active = value;
						r = layer;
						break;
					}
				}
			}
			if (result) break;
		}
		return result;  
	  },
	  getLayer:function(name){
		var obj = this;
		var groups = obj.options.layersList.groups;
		var result = null;
		for(var x in groups){
			var group = groups[x];
			var layers = group.layers;
			for (var y in layers){
				var layer = layers[y];
				if (y == name){
					var isTime = layer.time;
					if(layer.active && isTime){
						layer.idGroup =x;
						result = layer;	
						break;
					}
				}
			}
			if (result) break;
		}
		return result;
	  },
	  createItem:function(item){
			var obj = this;
		  	var activeLayers = obj.activeLayers;
		  	console.log(item);
		  
			var cadena = '';
		  	var layerId = item.id;
		    
		  	if (item){
					
				
				
					var size = 0;for(var x in item.years)size++;
					var years = item.years;
					var active = item.active;
					var width = obj.options.timeItemSize;//Math.floor(280/size);
						width = (width > 80)?80:width;
					//	cadena+= '<div class="linetime-yearsContainer">';
					var nameLayer = item.properties.label;
				    var haveSub = false;
					var count = 0;
				
				
					// Separar elmentos agrupandolos por fecha de ser posible
					var listYears = []; //vector para orden
					var _listYears = {}; //objeto para varios atributos
				
					for (var x in years){
							var layer = years[x]; 
							var year = x;
							var sub = year.indexOf('-');
							var parent = x;
							var _parent = x;
							var _sub = null;
						
							if(sub >= 0){
								_parent = x.split('-')[1];
								_sub = x.split('-')[0];
								
								haveSub = true; 
							}
						
							if(!_listYears[_parent]){ //si no esta guardado lo guarda
								listYears.push(_parent);
								_listYears[_parent] = {};
							}
						
							if(_sub){ //si existe sub determina que es un sublistado
								var isActive = false;
								for(var y in layer){
									if(layer[y].indexOf(active) >= 0)
										isActive = true;
								}
								
								//crea el padre de no existir
								if(!_listYears[_parent]['list'])
									_listYears[_parent] = {active:isActive,sub:true,list:[]};  // si no esta creada la lista la crea
								
								if(isActive)
									_listYears[_parent].active = true;
								
								_listYears[_parent].list.push({'name':_sub,layer:layer,sub:true,active:(layer.indexOf(active) >= 0)});
								
							}else{ //si no existe sub, determina que apunta de forma directa a la capa
								var isActive = false;
								for(var y in layer){
									for(var i in layer)
										if(layer[i] == active)
												isActive = true;
									//if(layer[y].indexOf(active) >= 0)
									//	isActive = true;
								}
								_listYears[_parent] = {'name':_parent,layer:layer,sub:false,active:isActive};
							}
						
							var isActive = (years[x] == active)?'linetime-item-selected':'';
							var oddEven = ((count % 2) == 0)?'even':'odd';
					}

					cadena = '<div id="linetime_'+item.id+'" class="linetime-n-item '+((haveSub)?'havesub':'')+'">'; //main
					cadena+= '	<div id="linetime_'+item.id+'_absolute" class="linetime-n-item-absolute"></div>'; 
					cadena+= '	<div class="linetime-n-item-leftContainer">'; //leftContainer
					cadena+= '		<div class="linetime-n-item-name">'+nameLayer+'</div>';
					cadena+= '		<div class="linetime-n-years-container">';
					//imresion de años
					if(listYears.length > obj.sizeElements)obj.sizeElements = listYears.length;
					
				
					for(var x in listYears){
						var _item = _listYears[listYears[x]];
						var label = listYears[x];
						var sub_label = '';
						if(_item.active){
							for(var y in _item.list){
								if(_item.list[y].active){
									sub_label = _item.list[y].name;
									break;
								}
							}
						}
						
						var attr_list = []; //se obtienen los subs
						if(attr_list.length > obj.sizeElements)obj.sizeElements = attr_list.length; //obtiene el tamaño
						
						for(var y in _item.list){
							var _sub = _item.list[y];
							attr_list.push(_sub.name+'|'+_sub.layer.join(' '));
						}
						
						cadena+= '		<div class="linetime-n-years-item '+
										((_item.active)?'active':'')+
										'" label="'+label+'" '+
										((attr_list.length > 0)?'subs="'+attr_list+'"':'')+
										' layerid="'+layerId+'" '+((!_item.sub)?'idref="'+_item.layer.join('')+'"':'')+'>';
						
						
						cadena+= 		'	<label class="linetime-n-label-year">'+label+'</label>';
						if(_item.sub){
							cadena+= 		'	<label class="linetime-n-label-year-sub">'+sub_label+'</label>';
						}
						cadena+= '		</div>';
					}
					
					cadena+= '		</div>';
					cadena+= '	</div>'; //leftContainer
					cadena+= '	<div class="linetime-n-item-rightContainer">'; //rightContainer
					cadena+= '		<div idref="'+item.id+'" class="timeline-play-layer timeline-icon timeLine-sprite timeLine_play"></div>';
					cadena+= '		<div idref="'+item.id+'" class="timeline-close-layer timeline-icon timeLine-sprite timeLine_eye"></div>';
					cadena+= '	</div>'; //rightContainer
					
					//Panel flotante para mostrar los sub elementos
					cadena+= '	<div class="linetime-n-float-container">'; //panel flotante
					
					cadena+= '	</div>';
					
					cadena+= '</div>'; //main
					
			}
			/*if (item){
					cadena = '<div id="linetime_'+item.id+'" class="linetime-item" index="e1"><div class="linetime-item-absolute">';
					
					cadena+='	<div class="timeline-lineContainer">';
					cadena+='		<div class="timeline-line"></div>';
					cadena+='		<div idref="'+item.id+'" class="timeline-play-layer timeline-icon timeLine-sprite timeLine_play"></div>';
					cadena+='		<div idref="'+item.id+'" class="timeline-close-layer timeline-icon timeLine-sprite timeLine_eye"></div>';
					cadena+='	</div>';
					
					var size = 0;for(var x in item.years)size++;
					var years = item.years;
					var active = item.active;
					var width = obj.options.timeItemSize;//Math.floor(280/size);
						width = (width > 80)?80:width;
						cadena+= '<div class="linetime-yearsContainer">';
					var count = 0;
					for (var x in years){
						
							var isActive = (years[x] == active)?'linetime-item-selected':'';
							var oddEven = ((count % 2) == 0)?'even':'odd';
							cadena+= '	<div id="'+item.id+'_'+x+'" idref="'+item.id+'" class="linetime-year '+isActive+'" style="width:'+width+'px;" alias="'+count+'">';
							cadena+= '		<div class="linetime-year-label disableSelection '+oddEven+' transition-fast" idref="'+item.id+'" alias="'+count+'">'+x+'</div>';
							cadena+= '  </div>';
							count++;
						
					}
					cadena+= '	</div>';
					var label = item.properties.label;
					//nombre de capa
					cadena+='<div class="timeline-layerName">'+label+'</div>';
					
					cadena+= '</div></div>';
				
			}*/
				
			return cadena;
	  },
	  createlayerRecord:function(id,layer,years){
		 var obj = this; 
			var labels =[];
			for(var x in years){
				labels.push(x);
			}
			var firstYear = years[labels[0]][0];
			var lastYear = years[labels[labels.length-1]][0];
			var layers = obj.activeLayers;
			var exists = false;
			for(var x in layers){
				if (layers[x].id == id){
					exists = true;
					break;
				}
			}
			if(!exists)
				return {id:id,properties:layer,years:years,active:lastYear}
			else
				return null;
	  },
	  createStructure:function(){
		var obj = this;  
		var data = obj.timeLayers;
		
		for(var x in data){
			var layer = obj.getLayer(x);
			if (layer){
				var tmpLayer = obj.createlayerRecord(x,layer,data[x].data);
				if (tmpLayer)
					obj.activeLayers.push(tmpLayer);
			}
		}
		var tl = obj.activeLayers;
		var maxNumItems = 0;
		for (var x in tl){
			var size = 0;
			for (var y in tl[x].years){
				size++;	
			}
		    if (size > maxNumItems)
				maxNumItems = size;
		}
		
		
		var height = tl.length*61;
		var width = 120;// obj.sizeElements*73;//+(90);
		
		var currentHeight = (!obj.firstRun)?0:height;
		var cadena ='<div class="timeline-leftSlider">';
			cadena+='	<div class="timeLine-sprite timeLine_down"></div>';
			cadena+='</div>';
			
			cadena+='<div class="timeline-layerContainer">';
		
			for (var x in tl){
				cadena+= obj.createItem(tl[x]);
			}
			cadena+='</div>';
		
		obj.element.html(cadena);
		  
		width = obj.sizeElements*70+(90);  
		  
		obj.element.attr('height',height).css(
							{
							'height':currentHeight+'px',
							'width':width+'px'
							});
		
		
		
		
		//reviza primer arranque
		if(!obj.firstRun)obj.firstRun = true;
		
		
		//asignacion de eventos
		$('.linetime-year-label').each(function(index, element) {
		   $(this).click(function(){
				var alias = $(this).attr('alias');
				var idref = $(this).attr('idref');
				obj.selectDate(alias,idref);
		   });
		});
		$('.timeline-leftSlider').click(function(){
			obj.hide();
		});
		
		$('.timeline-play-layer').each(function(index, element) {
		   $(this).click(function(){
				var idref = $(this).attr('idref');
				obj.openPlayer(idref);
		   });
		});
		
		
		$('.timeline-close-layer').each(function(index, element) {
		   $(this).click(function(){
				var idref = $(this).attr('idref');
				obj.internalDeactivate(idref);
		   });
		});
		  
		$('.linetime-n-years-item').each(function(){
			$(this).click(function(){
				var layerid = $(this).attr('layerid');
				var subList = $(this).attr('subs');
				var idref = $(this).attr('idref');
				if(subList){
					subList = subList.split(',');
					for(var x in subList){
						var item = subList[x].split('|');
						subList[x] = {
							name:item[0],
							layer:item[1]
						};
					}
					obj.closeAllPanelFloat();
					obj.printFloatPanel($(this).attr('layerid'),subList);
				}else{
					obj.selectDate(layerid,idref);
					obj.closeAllPanelFloat();
				}
			})
		});
	  },
      closeAllPanelFloat:function(){
			var obj = this;
		  $('.linetime-n-item-absolute').each(function(){
			 	$(this).html(''); 
		  });
	  },
	  printFloatPanel:function(layerid,list){
		  var obj = this;
		  var container = $('#linetime_'+layerid+'_absolute');
		  var cadena = '<div class="linetime-n-year-float-container" style="width:'+((list.length*52)+6)+'px">';
			  for(var x in list){
				  var item = list[x];
				  var layer = item.layer;
				  var name = item.name;
				  cadena+= '<div id="linetime_n_float_item_'+layer+'" layerid="'+layerid+'" idref="'+layer+'" label="'+name+'" class="linetime-n-year-float-item">'+name+'</div>';
			  }
		  	   		cadena+= '<div id="linetime_n_float_close" class="linetime-n-float-close"><div class="timeLine-sprite timeLine_close"></div></div>'
		  	  cadena+= '</div>';
		  
		 container.html(cadena);
		 $('.linetime-n-year-float-item').each(function(){
			$(this).click(function(){
				var layerid = $(this).attr('layerid');
				var idref = $(this).attr('idref');
				var label = $(this).attr('name');
				
				//$('#linetime_'+layerid+' .linetime-n-years-item[layerid='+idref+']' ).addClass('active');
				//$('#linetime_'+layerid+' .linetime-n-years-item[layerid='+idref+'] .linetime-n-label-year-sub' ).html(name);
				
				obj.selectDate(layerid,idref);
				obj.closeAllPanelFloat();
			}); 
		 });
		  
		 $('#linetime_n_float_close').click(function(){
			obj.closeAllPanelFloat();
		 });
		  
	  },
	  preloadPictures:function(pictureUrls, callback, progress, error) {
			var i,
				j,
				loaded = 0;
		
			for (i = 0, j = pictureUrls.length; i < j; i++) {
				(function (img, src) {
					img.onload = function () {
						if($.isFunction(progress))
							progress(Math.round((100/pictureUrls.length)*loaded)+'%');
							
						if (++loaded == pictureUrls.length && callback) {
							callback();
						}
					};
		
					// Use the following callback methods to debug
					// in case of an unexpected behavior.
					img.onerror = function () {
						if($.isFunction(error))
							error();	
					};
					img.onabort = function () {};
		
					img.src = src;
				} (new Image(), pictureUrls[i]));
			}
	  },
	  init:function(){
		var obj = this;
		obj.timeLayers = obj.getData();
		obj.createStructure();
	  },
	  selectDate:function(alias,idref){ //alias layer, idref new select
		var obj = this;
		
		var layers = obj.activeLayers;
		var changed = false;
		for(var x in layers){
			if (layers[x].id == alias){
				//var pos = parseInt(alias,10);
				var newSubLayer = idref;//layers[x].years[Object.keys(layers[x].years)[pos]].join();
				changed = (layers[x].active != newSubLayer)
				if (changed){
					//desactiva capa anteriormente activa
					var treeLayer = $.extend({},obj.getLayer(layers[x].id));
					treeLayer.idLayer = layers[x].active;
					obj.externalDeactivate(treeLayer);
					
					layers[x].active = idref;//layers[x].years[Object.keys(layers[x].years)[pos]].join();
					
					//activa nueva capa
					treeLayer.idLayer = layers[x].active;
					obj.externalActivate(treeLayer);	 
					obj.createStructure(); //obliga que se cree el contenido al cambiar una cap
				}
				break;
			}
		}
		if(changed){
				/*
				$('#linetime_'+alias+' .active').each(function(){
					$(this).removeClass('active');								 
				});

				$('#linetime_'+alias+' [idref='+idref+']').each(function(){
					$(this).addClass('active')
				});	
				*/
		//	$('#linetime_'+idref).attr('index',alias);  
			
			
			/*$('.linetime-year[idref='+idref+'].linetime-item-selected').removeClass('linetime-item-selected');
			$('.linetime-year[idref='+idref+'][alias='+alias+']').addClass('linetime-item-selected');*/
		}
	  },
      // the constructor
      _create: function() {
        var obj = this;
        //obj.options.currentYear = (obj.options.currentYear > 0)?obj.options.currentYear:obj.options.minYear;
        
        this.element
          // add a class for theming
          .addClass( "custom-timeline no-print" )
          // prevent double click to select text
          .disableSelection();
        // bind click events on the changer button to the random method
        this.id = this.element.attr('id');
		obj.init();
		
		$(window).resize(function(){
			var idref =  $('#timeline_player').attr('idref');
			if(idref)
				obj.closePlayer(idref);  
		});
		
        this._refresh();
      },
      
      // called when created, and later when changing options
      _refresh: function(){
        // trigger a callback/event
        this._trigger( "change" );
      },
      // revert other modifications here
      _destroy: function() {
        this.options.close();
        this.element.remove();
      },
 
      // _setOptions is called with a hash of all options that are changing
      // always refresh when changing options
      _setOptions: function() {
        // _super and _superApply handle keeping the right this-context
        this._superApply( arguments );
        this._refresh();
      },
 
      // _setOption is called for each individual option that is changing
      _setOption: function( key, value ) {
        var obj = this;
        this._super( key, value );
      },
	  getData:function(func){
		var obj = this;  
		var groups = obj.options.layersList.groups;
		var r = {};
		for(var x in groups){
			var group = groups[x];
			var layers = group.layers;
			for (var y in layers){
				var layer = layers[y];
				var isTime = layer.time;
				if(isTime){
					r[y] = {data:layer.dates}
				}
			}
		}
		return r;
	  }
    });// JavaScript Document// JavaScript Document