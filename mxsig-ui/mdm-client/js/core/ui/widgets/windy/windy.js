define(["dataSource","printControl","toolsConfig","tree"], function(dataSource,printControl,toolsConfig,tree){
$.widget( "custom.windy", {
		  id:'custom_windy',
          created:false,
          animate:true,
          collapsed:true,
          maxHeight:270,//550
          minHeight:200,
          root:'body',
          visible:false,
          extentByDefault:true,
          data:null,
          dataSelected:{
                    date:{value:'09/01/2018'},
                    clock:{label:0,value:'12',measure:'hrs'},
                    surface:{value:'100',measure:'ha'}
                    
          },
          calendar:{},
          refCalendar:{},
          legend:[
                    {label:'0 km/h',color:'#97CEE2'},
                    {label:''/*'36 km/h'*/,color:'#AEC7E6'},
                    {label:''/*'72 km/h*/,color:'#B6B6DE'},
                    {label:''/*'108 km/h*/,color:'#AD92C4'},
                    {label:'144 km/h',color:'#A3549D'}
          ],
		  _init:function(){
					
		  },
          removeEmptySpaces:function(a){
                    a = a+'';
                    return a.replace(/\s+/, "");
          },
		  buildStructure:function(){
                         var obj = this;
						var chain = '<div id="'+this.id+'" class="windy_animated '+this.id+' no-print" collapsed="'+obj.collapsed+'" selectordate="false">'+
                                            '<div class="content">'+
                                                  '<div class="header">'+
                                                            '<div class="logo"><div class="template_windy tw_logo"></div></div>'+
                                                            '<div class="titlesWindy">'+
                                                                      '<div class="title">Predicción de vientos</div>'+
                                                            '</div>'+
                                                            '<div class="plegTable">'+
                                                                      '<div class="template_windy tw_custom"></div>'+/*tw_rowDown*/
                                                            '</div>'+
                                                            '<div class="Resume windy_animated">'+obj.getInfoSelected()+'</div>'+
                                                            obj.getLegend()+
                                                  '</div>'+
                                                  '<div class="container windy_animated">'+
                                                            '<div class="sectionTool Date windy_animated">'+
                                                                                '<div class="Icon">'+
                                                                                          '<div class="template_windy tw_date"></div>'+
                                                                                          '<div class="Label">Fecha</div>'+
                                                                                '</div>'+
                                                                                '<div class="Edit"><div class="template_windy tw_edit"></div></div>'+
                                                                                '<div class="Selector">herramienta</div>'+
                                                            '</div>'+
                                                            '<div class="sectionTool Clock windy_animated">'+
                                                                                '<div class="Icon">'+
                                                                                          '<div class="template_windy tw_clock"></div>'+
                                                                                          '<div class="Label">Hora</div>'+
                                                                                '</div>'+
                                                                                '<div class="Selector">herramienta</div>'+
                                                            '</div>'+
                                                            '<div class="sectionTool Surface windy_animated">'+
                                                                                '<div class="Icon">'+
                                                                                          '<div class="template_windy tw_surface"></div>'+
                                                                                          '<div class="Label">Altitud</div>'+
                                                                                '</div>'+
                                                                                
                                                                                '<div class="Selector">herramienta</div>'+
                                                            '</div>'+
                                                            '<div class="selectorDate">'+
                                                                      '<div class="Back"><div class="template_windy tw_back"></div></div>'+
                                                                      '<div class="Content">'+
                                                                                
                                                                      '</div>'+
                                                            '</div>'+
                                                  '</div>'+
                                            '</div>'+
											
                                            '<div class="blocker"><center class="containerBlocker"><div class="etiquetaBlock">Cargando</div><div><img src="js/core/ui/widgets/windy/spinner.gif"></div></center></div>'+
								   '<div>';
						$('body').append(chain);
                        this.created=true;
                        this.visible=true;
		  },
          getDay:function(pos){
                    var weekday=[];
                    weekday[1] = "Do";
                    weekday[2] = "Lu";
                    weekday[3] = "Ma";
                    weekday[4] = "Mi";
                    weekday[5] = "Ju";
                    weekday[6] = "Vi";
                    weekday[7] = "Sa";
                    return weekday[pos];
          },
          getCalendar:function(){
                    var chain = '';
                    var obj = this;
                    var calendar = {};
                    var array=[];
                    obj.calendar=null;
                    obj.calendar={};
                    obj.refCalendar={};
                    var row = 1;
                    var selectedDate =obj.dataSelected.date.value.split('/');
                    var monthSelected = obj.getMonth(parseInt(selectedDate[1]));
                    for(var x in obj.data.infoDays){
                              var textDate = obj.getDate(obj.data.infoDays[x].date);
                              var d = new Date(textDate);
                              var sameMonth=true;
                              var n = d.getDay()+1;
                              var m = d.getMonth()+1;
                              var number = d.getDate();          
                              var label = obj.getMonth(m);
                              
                              if (!calendar[label]) {
                                calendar[label]=[];
                              }
                              array[n]={day:number,date:obj.data.infoDays[x].date,ref:row+x};
                              obj.calendar[obj.data.infoDays[x].date]={ref:row+x,pos:x};
                              obj.refCalendar['r'+row+x]=obj.data.infoDays[x].date+'';
                              row+=1;
                              if (obj.data.infoDays[parseInt(x)+1]) {
                                var current =obj.data.infoDays[x].date.split('/');        
                                var next = obj.data.infoDays[parseInt(x)+1].date.split('/');
                                if (current[1]!=next[1]) {
                                    sameMonth=false;
                                }
                              }
                              if ((n==7)||(x==obj.data.infoDays.length-1)||(!sameMonth)) {
                                calendar[label].push(array);
                                array = null;
                                array=[];
                                row=1;
                              }
                    }
                    var months = '';
                    var totalItems=0;
                    for(var x in calendar){
                              var block = calendar[x];
                              months+='<option class="itemMonth" value="'+x+'">'+x+'</option>';
                              chain+='<div class="Calendar '+x+'" class="'+((monthSelected==x)?'Selected':'')+'">';
                              for(var y in block){
                                        var row = block[y];
                                        var titles = '';
                                        var rowChain = '';
                                        for(var i = 1;i<=7;i++){
                                                  var value = '0';
                                                  var clase = 'itemOff';
                                                  var selected='';
                                                  var reference = '';
                                                  var position = '';
                                                  if (row[i]) {
                                                            value = row[i].day;
                                                            clase = 'itemNumber';
                                                            selected = (row[i].date==obj.dataSelected.date.value)?' selected':'';
                                                            reference=row[i].ref;
                                                            position=row[i].pos;
                                                  }
                                                  
                                                  if (y==0) {
                                                    titles+='<div class="itemCell itemDay">'+obj.getDay(i)+'</div>';
                                                  }
                                                  
                                                  rowChain+='<div class="itemCell '+clase+selected+'" ref="'+reference+'" pos="'+position+'">'+value+'</div>';
                                                  
                                        }
                                        chain+=((titles!='')?'<div class="itemRow">'+titles+'</div>':'')+'<div class="itemRow">'+rowChain+'</div>';
                              }
                              chain+='</div>';
                              totalItems+=1;
                    }
                    var specialClass = (totalItems==1)?'unique':'';
                    return '<select class="monthTab '+specialClass+'" '+((totalItems==1)?' disabled ':'')+'>'+months+'</select>'+chain;
          },
          getInfoSelected:function(){
                    var chain='';
                    var obj = this;
                    for(var x in obj.dataSelected){
                            var i = obj.dataSelected[x];
                            chain +='<div class="itemInfo">'+
                                                  '<div class="template_windy tw_min_'+x+'"></div>'+
                                                  '<div class="info_'+x+'">'+i.value+((i.measure)?' '+i.measure:'')+'</div>'+
                                    '</div>';
                    }
                    return chain;
          },
          getLegend:function(){
                    var obj = this;
                    var width=100/obj.legend.length;
                    var labels = '';
                    var colors='';
                    for(var x in obj.legend){
                                        var i=obj.legend[x];
                                        labels+='<div class="itemLegendvalue" style="width:'+width+'%;height:15px;"><div>'+i.label+'</div></div>';
                                        colors+='<div class="itemLegend" style="background:'+i.color+';width:'+width+'%;height:15px;"></div>';
                    }
                    var chain='<div class="Legend">'+
                                        '<div class="values">'+labels+'</div>'+
                                        '<div class="colors">'+colors+'</div>'+
                              '</div>';
                    return chain;
          },
          hide:function(){
                    var obj = this;
                    $("."+obj.id).fadeOut();
                    obj.visible=false;
          },
          show:function(){
                    var obj = this;
                    $("."+obj.id).fadeIn();
                    obj.visible=true;
          },
          getFormatNumber : function(nStr){
                    nStr += '';
                    x = nStr.split('.');
                    x1 = x[0];
                    //alert('antes');
                    x2 = x.length > 1 ? '.' + x[1] : '';
                    var rgx = /(\d+)(\d{3})/;
                    while (rgx.test(x1)) {
                        x1 = x1.replace(rgx, '$1' + ',' + '$2');
                    }
                    return x1 + x2;
          },
          
          request : function(params,action){
                  obj=this;
                  var msg = 'Servicio no disponible intente m&aacute;s tarde';
                  var r= {
                            success:function(json,estatus){
                                var valid=false;
                                if (json){
                                        var valid = json.response.success;
                                            if(valid){
                                                  valid = true;
                                                  switch (action) {
                                                            case 'getInfo':
                                                                      obj.data=json.data.info;
                                                                      obj.fillInformation();
                                                                      
                                                            break;
                                                            case 'display':
                                                                      var i = json.data.info;
                                                                      obj.setData({labelClock:i.hour,clock:i.forecastTime,surface:i.surfaceValue,date:i.date},false);
                                                                      MDM6('enableWindy',true,json.data.winds);
                                                            break;
                                                  }
                                                  
                                            }
                                }
                                if (!valid) {
                                         //alert(msg);
                                }
                                
                            },
                            beforeSend: function(xhr) {
                                obj.showSpinner();
                               
                            },
                            error: function(solicitudAJAX,errorDescripcion,errorExcepcion) {
                                        //alert('Error')
                            },
                            complete: function(solicitudAJAX,estatus) {     
                                
                                if ((action=='getInfo')&&(obj.data)) {
                                    obj.setData(null,true);
                                }
                                obj.hideSpinner();
                            }
                  };
                  var source = dataSource.windy[action];
                  r = $.extend(r, source);
                  params = (r.params)?($.extend(r.params,params)):params;
                  r.data = (source.stringify)? JSON.stringify(params):params;
                  $.ajax(r);
          },
          selectMonth:function(monthSelected){
                    var obj = this;
                    $("#"+obj.id+" .selectorDate .monthTab").val(monthSelected);
                    $("#"+obj.id+" .selectorDate .Calendar.Selected").removeClass('Selected');
                    $("#"+obj.id+" .selectorDate .Calendar."+monthSelected).addClass('Selected');
          },
          setData:function(params,reload){
                    var obj = this;
                    var sel =obj.dataSelected;
                    if (params) {
                              if (params.clock) {
                                  var i = sel.clock;
                                  i.value=params.clock+'';
                                  $("#"+obj.id+" .Clock .Selector").children('select').val(i.value);
                                  var i = sel.clock;
                                  //$("#"+obj.id+" .Resume .info_clock").html(i.value+((i.measure)?' '+i.measure:''));
                              }
                              if (params.labelClock) {
                                  var i = sel.clock;
                                  i.label=parseInt(params.labelClock);
                                  //$("#"+obj.id+" .Clock .Selector").children('select').val(i.value);
                                  var i = sel.clock;
                                  $("#"+obj.id+" .Resume .info_clock").html(i.label+((i.measure)?' '+i.measure:''));
                              }
                              if (params.surface) {
                                  var i = sel.surface;      
                                  i.value=params.surface+'';
                                  $("#"+obj.id+" .Surface .Selector .item.selected").removeClass('selected');
                                  $("#"+obj.id+" .Surface .Selector .item[pos='"+i.value+"']").addClass('selected');
                                  $("#"+obj.id+" .Resume .info_surface").html(i.value+((i.measure)?' '+i.measure:''));
                              }
                              if (params.date) {
                                  var i = sel.date;      
                                  i.value=params.date+'';
                                  $("#"+obj.id+" .Date .Selector").html(obj.getDateTextFormat(i.value));
                                  $("#"+obj.id+" .Resume .info_date").html(i.value+((i.measure)?' '+i.measure:''));
                                  $("#"+obj.id+" .selectorDate .Content .selected").removeClass('selected');
                                  $("#"+obj.id+" .selectorDate .Content div[ref='"+obj.calendar[i.value].ref+"']").addClass('selected');
                                  
                                  var selectedDate =i.value.split('/');
                                  var monthSelected = obj.getMonth(parseInt(selectedDate[1]));
                                  obj.selectMonth(monthSelected);
                              }
                    }
                    if (reload) {
                        var Params ={forecastTime:sel.clock.value,surfaceValue:sel.surface.value};
                        obj.request(Params,'display');
                    }
          },
          getDate:function(text){
                    var response = ''
                    var a = text.split('/');
                    response=a[1]+'/'+a[0]+'/'+a[2];
                    return response;
          },
          getCurrentDate:function(){
                    var obj = this;
                    var d = new Date();
                    var month = d.getMonth()+1;
                    var day = d.getDate();
                    return (day<10 ? '0' : '') + day +'/'+
                    (month<10 ? '0' : '') + month + '/' +
                    d.getFullYear();
          },
          getCurretHour:function(){
                    var currentdate = new Date(); 
                    return currentdate.getHours();// + ":"+ currentdate.getMinutes() + ":" + currentdate.getSeconds();
          },
          getMonth:function(pos){
                    var month = new Array();
                    month[1] = "Enero";
                    month[2] = "Febrero";
                    month[3] = "Marzo";
                    month[4] = "Abril";
                    month[5] = "Mayo";
                    month[6] = "Junio";
                    month[7] = "Julio";
                    month[8] = "Agosto";
                    month[8] = "Septiembre";
                    month[10] = "Octubre";
                    month[11] = "Noviembre";
                    month[12] = "Diciembre";
                    return month[pos];
          },
          fillInformation:function(){
                    var obj = this;
                    var surface = '';
                    for(var x in obj.data.surfaceValues){
                              var i = obj.data.surfaceValues[x];
                              if (parseInt(x)==0) {
                                obj.dataSelected.surface.value=i+'';
                              }
                              surface+='<div class="item" pos="'+i+'">'+i+' '+obj.dataSelected.surface.measure+'</div>';
                    }
                    $("#"+obj.id+" .Surface .Selector").html(surface);
                    var currentDate = obj.getCurrentDate();
                    var currentHour = obj.getCurretHour();
                    var infoDate = obj.defineClock(currentHour,currentDate);
                    $("#"+obj.id+" .Date .Selector").html(obj.getDateTextFormat(infoDate.date));
                    $("#"+obj.id+" .Surface .Selector .item").click(function(){
                              var value = $(this).attr('pos');
                              obj.setData({surface:value},false);
                              obj.setData(null,true);
                    });
                    $("#"+obj.id+" .selectorDate .Content").html(obj.getCalendar());
                    $("#"+obj.id+" .selectorDate .monthTab").change(function(){
                              var monthSelected = $(this).val();
                              obj.selectMonth(monthSelected);
                              obj.setData(null,true);
                    });
                    $("#"+obj.id+" .selectorDate .itemNumber").click(function(){
                              var ref = $(this).attr('ref');
                              obj.redefineInformation(obj.refCalendar['r'+ref]);
                              obj.setData({date:obj.refCalendar['r'+ref]},false);
                              obj.setData(null,true);
                    });
          },
          defineClock:function(currentHour,currentDate,redefine){
                    var obj = this;
                    currentHour =parseInt(currentHour);
                    var infoDate=null;
                    for(var x in obj.data.infoDays){
                              var i = obj.data.infoDays[x];
                              if (i.date==currentDate) {
                                        infoDate=i;
                                        break;
                              }
                              if( ((new Date(i.date).getTime() > new Date(currentDate).getTime()))){
                                        infoDate=obj.data.infoDays[x-1];
                                        break;
                              }
                    }
                    
                    var clock='<select>';
                    var hourSelected=false;
                    for(var x in infoDate.hours){
                              var hour = infoDate.hours[x].hour;
                              var forecast= infoDate.hours[x].forecastTime;
                              var selected='';
                              if ((!hourSelected)&&((hour==currentHour)||((infoDate.hours[(parseInt(x))+1])&&(infoDate.hours[(parseInt(x))+1].hour>currentHour)))) {
                                    selected='selected="selected"';
                                    hourSelected=true;
                                    obj.dataSelected.clock.value=forecast+'';
                              }
                              hour = (hour<10)?'0'+hour:hour;
                              hour+=':00';
                              clock+='<option '+selected+' c="'+infoDate.hours[x].hour+'" value="'+infoDate.hours[x].forecastTime+'">'+hour+' '+obj.dataSelected.clock.measure+'</option>';
                    }
                    clock+='</select>';
                    $("#"+obj.id+" .Clock .Selector").html(clock);
                    $("#"+obj.id+" .Clock .Selector").children('select').change(function(){
                              var value = $(this).val();
                              var label = ($("#"+obj.id+" .Clock .Selector select option:selected").attr('c'))+'';
                              obj.setData({clock:value,labelClock:label},false);
                              obj.setData(null,true);
                    });
                    if ((redefine)&&(!hourSelected)) {
                              var value = $("#"+obj.id+" .Clock .Selector").children('select').val();
                              var label = ($("#"+obj.id+" .Clock .Selector select option:selected").attr('c'))+'';
                              obj.setData({clock:value,labelClock:label},false);
                    }
                    return infoDate;
          },
          redefineInformation:function(date){
                    var obj = this;
                    var item = obj.data.infoDays[obj.calendar[date].pos];
                    obj.defineClock(obj.dataSelected.clock.label,date,true);
                    
                    
          },
          getDateTextFormat:function(params){
                    var result='';
                    var chain = params.split('/');
                    var month = parseInt(chain[1]);
                    switch (month) {
                        case 1:Month = 'Enero';
                              break;
                        case 2:Month = 'Febrero';
                              break;
                        case 3:Month = 'Marzo';
                              break;
                        case 4:Month = 'Abril';
                              break;
                        case 5:Month = 'Mayo';
                              break;
                        case 6:Month = 'Junio';
                              break;
                        case 7:Month = 'Julio';
                              break;
                        case 8:Month = 'Agosto';
                              break;
                        case 9:Month = 'Septiembre';
                              break;
                        case 10:Month = 'Octubre';
                              break;
                        case 11:Month = 'Noviembre';
                              break;
                        case 12:Month = 'Diciembre';
                              break;
                    }
                    return chain[0]+" de "+Month+" del "+chain[2];
          },
          //?forecastTime=-24&surfaceValue=10
          hideSpinner:function(){
                    $("."+obj.id +" .blocker").fadeOut();
          },
          showSpinner:function(){
                    var obj = this;
                    $("."+obj.id +" .blocker").fadeIn();
          },
          convertToHtml : function(texto){
		
                    var ta=document.createElement("textarea");
                    ta.innerHTML=texto.replace(/</g,"&lt;").replace(/>/g,"&gt;");
                    return ta.value;
          },
          replaceSpetialCharacters:function(text){
                    return text.replace(/[^\w\s]/gi, '_');
          },
		  events:function(){
						var obj = this;
                        $("."+obj.id +" .plegTable").click(function(){
                             obj.pleg();
                             if (obj.collapsed) {
                                //$("."+obj.id+" .selectorDate").fadeOut();
                                //obj.setData(null,true);
                             }
                        });
                        $("."+obj.id+" .Back").click(function(){
                              $('.'+obj.id).attr('selectordate',false);
                              $("."+obj.id+" .selectorDate").hide( 'slide', {direction:'right'}, 500, function(){} );
                        });
                        $("."+obj.id+" .container .Date").click(function(){
                              $("."+obj.id+" .selectorDate").show( 'slide', {direction:'right'}, 500, function(){$('.'+obj.id).attr('selectordate',true);} );
                        });
                        
                        $( window ).resize(function() {
                                        if (!obj.collapsed) {
                                                  var height = ($(window).height() < (270+230))?$(window).height()-230:270;
                                                  obj.maxHeight = height;
                                                  $("."+obj.id).css('height',height+'px');
                                        }
                        });
                        
		  },
          pleg:function(){
                    var obj = this;
                    obj.collapsed = !obj.collapsed;
                    var newSide = (obj.collapsed)?'custom':'rowDown';
                    $('.'+obj.id+' .plegTable').children('div').removeClass('tw_custom').removeClass('tw_rowDown');
                    $('.'+obj.id+' .plegTable').children('div').addClass('tw_'+newSide);
                    $('.'+obj.id).attr('collapsed',obj.collapsed);
          },

		  update:function(applyActual){
                    var obj = this;
                    if (!obj.created) {
                              obj.goToExtent();
                              obj.buildStructure();
                              obj.events();
                    }
                    obj.show();
                    if (applyActual) {
                              this.reset();
                              var sel =obj.dataSelected;
                              var Params ={forecastTime:sel.clock.value,surfaceValue:sel.surface.value};
                              this.request(Params,'display');    
                    }else{
                              this.request({},'getInfo');
                    }
		  },
          reset:function(){
                    var obj = this;
                    obj.collapsed = true;
                    //$("."+obj.id).css('height','79px');
                    $('.'+obj.id+' .plegTable').children('div').removeClass('tw_custom').removeClass('tw_rowDown');
                    $('.'+obj.id+' .plegTable').children('div').addClass('tw_custom');
                    $('.'+obj.id).attr('collapsed',obj.collapsed);
                    
          },
          goToExtent:function(){
                    var obj = this;
                    if (obj.extentByDefault) {
                        MDM6('initialExtent');
                    }
          },
		  _create: function() {
                       this.update();
		  },
		  
		  _refresh: function(){
				// trigger a callback/event
				this._trigger( "change" );
		  },
			 
		  _destroy: function() {
				  this.element.remove();
		  },
		
		  setOptions: function() {
				// _super and _superApply handle keeping the right this-context
				this._superApply( arguments );
				this._refresh();
		  },
		  _setOption: function(key, value){
						this.options[key] = value;
						switch(key){
								case 'enable':
											this.options.enable=value;
                                            this.goToExtent();
                                            if (this.options.enable) {
                                                this.update(true);
                                            }else{
                                                  obj.hide();
                                                  MDM6('enableWindy',null);   
                                            }
								break;
						}
		  }
});
});