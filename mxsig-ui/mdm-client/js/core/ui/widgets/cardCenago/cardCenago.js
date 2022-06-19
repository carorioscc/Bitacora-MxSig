define(["dataSource","printControl"], function(dataSource,printControl){
$.widget( "custom.cardCenago", {
		  id:'custom_cardCenago',
          created:false,
          idForm:'custom_cardCenago_form',
          dataToDownload:null,
		  interpreter:[{value:'-9',label:'ND'},{value:'-10',label:'NA'},{value:'-7',label:'NS'},{value:'-8',label:'(ND/NS)'}],
          valueHeader:{
                    "Hombres":"gob_rh_pap_hom",
                    "Mujeres":"gob_rh_pap_muj"
          },
          textDownload:{
                    'edomun':'Fuente: Censo Nacional de Gobierno, Seguridad Pública y Sistema Penitenciario Estatales 2015 y  Censo Nacional de Gobiernos Municipales y Demarcaciones territoriales 2015.',
                    'edo':'Fuente: Censo Nacional de Gobierno, Seguridad Pública y Sistema Penitenciario Estatales 2015.',
                    'mun':'Fuente: Censo Nacional de Gobiernos Municipales y Demarcaciones territoriales 2015.'
          },
          additionalFields:{
                    "gob_rh_pap":"Personal en las instituciones de las administraciones públicas"
          },
          tree:{
                    "regime":[
                                    "gob_rh_pap__rc_conf",
                                    "gob_rh_pap__rc_base",
                                    "gob_rh_pap__rc_even",
                                    "gob_rh_pap__rc_hon",
                                    "gob_rh_pap__rc_otro",
                                    "gob_rh_pap__rc_ne"
                                    
                    ],
                    "security":[
                                    "gob_rh_pap__iss_issste",
                                    "gob_rh_pap__iss_issef",
                                    "gob_rh_pap__iss_imss",
                                    "gob_rh_pap__iss_otra",
                                    "gob_rh_pap__iss_sinss",
                                    "gob_rh_pap__iss_ne"
                    ],
                    "age":[
                                    "gob_rh_pap__edad_18a24",
                                    "gob_rh_pap__edad_25a29",
                                    "gob_rh_pap__edad_30a34",
                                    "gob_rh_pap__edad_35a39",
                                    "gob_rh_pap__edad_40a44",
                                    "gob_rh_pap__edad_45a49",
                                    "gob_rh_pap__edad_50mas",
                                    "gob_rh_pap__edad_ne"
                    ],
                    "entry":[
                                    "gob_rh_pap__ingreso_sin",
                                    "gob_rh_pap__ingreso_menos5",
                                    "gob_rh_pap__ingreso_5a10",
                                    "gob_rh_pap__ingreso_10a15",
                                    "gob_rh_pap__ingreso_15a20",
                                    "gob_rh_pap__ingreso_20a25",
                                    "gob_rh_pap__ingreso_25a30",
                                    "gob_rh_pap__ingreso_30a35",
                                    "gob_rh_pap__ingreso_35a40",
                                    "gob_rh_pap__ingreso_40a45",
                                    "gob_rh_pap__ingreso_45a50",
                                    "gob_rh_pap__ingreso_mas50",
                                    "gob_rh_pap__ingreso_ne"
                    ]
                    
          },
          indicador: {
                    "age":"Edad",
                    "entry":"Ingreso mensual",
                    "regime":"Régimen de contratación",
                    "security":'Seguridad social',
                    "gob_rh_pap__rc_base": "Base o sindicalizado",
                    "gob_rh_pap__rc_even": "Eventual",
                    "gob_rh_pap__rc_hon": "Honorarios",
                    "gob_rh_pap__rc_otro": "Otro",
                    "gob_rh_pap__rc_ne": "No especificado ",
                    "gob_rh_pap__iss_issste": "Instituto de Seguridad y Servicios Sociales de los Trabajadores del Estado (ISSSTE)",
                    "gob_rh_pap__iss_issef": "Institución de Seguridad Social de la Entidad Federativa u homóloga",
                    "gob_rh_pap__iss_imss": "Instituto Mexicano del Seguro Social (IMSS)",
                    "gob_rh_pap__iss_otra": "Otra institución de Seguridad Social",
                    "gob_rh_pap__iss_sinss": "Sin seguridad social",
                    "gob_rh_pap__iss_ne": "No especificado ",
                    "gob_rh_pap__edad_18a24": "18 a 24 años",
                    "gob_rh_pap__edad_25a29": "25 a 29 años",
                    "gob_rh_pap__edad_30a34": "30 a 34 años",
                    "gob_rh_pap__edad_35a39": "35 a 39 años",
                    "gob_rh_pap__edad_40a44": "40 a 44 años",
                    "gob_rh_pap__edad_45a49": "45 a 49 años",
                    "gob_rh_pap__edad_50mas": "50 ó más años",
                    "gob_rh_pap__edad_ne": "No especificado ",
                    "gob_rh_pap__ingreso_sin": "Sin paga",
                    "gob_rh_pap__ingreso_menos5": "Menos de 5 000 pesos",
                    "gob_rh_pap__ingreso_5a10": "De 5 001 a 10 000 pesos",
                    "gob_rh_pap__ingreso_10a15": "De 10 001 a 15 000 pesos",
                    "gob_rh_pap__ingreso_15a20": "De 15 001 a 20 000 pesos",
                    "gob_rh_pap__ingreso_20a25": "De 20 001 a 25 000 pesos",
                    "gob_rh_pap__ingreso_25a30": "De 25 001 a 30 000 pesos",
                    "gob_rh_pap__ingreso_30a35": "De 30 001 a 35 000 pesos",
                    "gob_rh_pap__ingreso_35a40": "De 35 001 a 40 000 pesos",
                    "gob_rh_pap__ingreso_40a45": "De 40 001 a 45 000 pesos",
                    "gob_rh_pap__ingreso_45a50": "De 45 001 a 50 000 pesos",
                    "gob_rh_pap__ingreso_mas50": "Más de 50 000 pesos",
                    "gob_rh_pap__ingreso_ne": "No especificado ",
                    "gob_rh_pap_muj": "Mujeres",
                    "gob_rh_pap__rc": "Por régimen de contratación",
                    "gob_rh_pap__rc_conf": "Confianza",
                    "gob_rh_pap__iss": "Por institución de seguridad social en el que se encontraba registrado",
                    "gob_rh_pap__edad": "Por rango de edad",
                    "gob_rh_pap__ingreso": "Por rango de ingreso mensual",
                    "gob_rh_pap_hom": "Hombres",
                    "gob_rh_pap": "Personal en las instituciones de las administraciones públicas",
          },
          root:'body',
          visible:false,
		  options:{
                    
                    find:{
                                 point:"POINT(-11389070.482837 2501073.0852542)",
                                 cvegeo:"00", 
                                 level:"edo"

                    },
                    card:{
                                 id:0,
                                 level:"mun",
                                 point:"POINT(-11389070.482837 2501073.0852542)",
                                 resolution:"152.874056542",
                                 year:"2014"        
                    },
                    title:'Censos de gobierno',
                    connection:null,
                    typeSelected:null
                     
		  },
          additionText:{
                    'edomun':'  estatales, municipales y demarcaciones territoriales',
                    'edo':' estatales',
                    'mun':' municipales y demarcaciones territoriales'
          },
		  _init:function(){
						
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
          getValueSection:function(value){
                    var obj = this;
                    var response = value;
                    if (obj.indicador[value]) {
                        response = obj.indicador[value];
                    }
                    return response;
          },
		  getValue:function(value){
				    var response = value;
					var obj = this;
					var i = obj.interpreter;
					for(var x in i){
						  if (i[x].value==(value+'')) {
							  response=i[x].label;
							  break;
                          }
					}
					return response;
		  },
          getTools:function(){
                    var obj = this;
                    var  chain='<div class="tools">'+
                                        '<div class="close itemTool">'+
                                                '<div class="dinamicPanel-sprite dinamicPanel-close-short"></div>'+
                                        '</div>'+
                                        '<div class="download itemTool">'+
                                                '<div class="sprite_infoCard spriteDownload"></div>'+
                                        '</div>'+
                                        '<div class="print itemTool">'+
                                                '<div class="recordCard_template print_item"></div>'+
                                        '</div>'+
                                '</div>';
                    return chain;
          },
		  buildStructure:function(){
                              
						var chain = '<div id="'+this.id+'" class="'+this.id+' no-print">'+
											'<div class="container"></div>'+
                                            '<div class="blocker"><center class="containerBlocker"><div class="etiquetaBlock">Cargando</div><div><img src="js/core/ui/widgets/cardCenago/spinner.gif"></div></center></div>'+
                                            obj.getTools()+
								   '<div>';
						$('body').append(chain);
                        this.created=true;
                        this.visible=true;
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
          request : function(params,mode){
                  obj=this;
                  var msg = 'Servicio no disponible intente m&aacute;s tarde';
                  var r= {
                            success:function(json,estatus){
                                var valid=false;
                                if (json){
                                        if (mode=='find') {
                                            if(json.response.success){
                                                  valid = true;
                                                  if (!obj.created) {
                                                            obj.buildStructure();
                                                            obj.events();
                                                  }
                                                  var center = (!obj.visible)?true:false;
                                                  obj.show();
                                                  if (center) {
                                                      obj.centerWindow();
                                                  }
                                                  
                                                  obj.options.data.card.id = json.data.values.cvegeo;
                                                  obj.request(obj.options.data.card,'card');
                                            }else{
                                                  obj.options.data.event();
                                            }
                                        }else{
                                                   valid=true;
                                                  obj.displayInformation(json);
                                        }
                                }
                                if (!valid) {
                                         //alert(msg);
                                }
                                
                            },
                            beforeSend: function(xhr) {
                              if (mode!='find') {
                                obj.showSpinner();
                              }
                               
                            },
                            error: function(solicitudAJAX,errorDescripcion,errorExcepcion) {
                                        //alert('Error')
                            },
                            complete: function(solicitudAJAX,estatus) {
                              if (mode!='find') {
                                obj.hideSpinner();
                              }
                               
                                
                            }
                  };
                  var source = dataSource.cenago[mode];
                  r = $.extend(r, source);
                  r.data = (source.stringify)? JSON.stringify(params):params;
                  $.ajax(r);
          },
          centerWindow:function(){
                    var obj = this;
                    $("."+obj.id).position({
                              of: $( obj.root ),
                              my: "center center",
                              at: "center center"
                    });
          },
          hideSpinner:function(){
                    $("."+obj.id +" .blocker").fadeOut();
          },
          showSpinner:function(){
                    var obj = this;
                    $("."+obj.id +" .blocker").fadeIn();
          },
          findVariable:function(source,variable){
                    var response = {exist:false,value:0};
                    for(var x in source){
                              var i = source[x];
                              if (i.label==variable) {
                                response.exist=true;
                                response.value=i.value;
                                break;
                              }
                    }
                    return response;
          },
          showBlock:function(section){
                    var obj = this;
                    var active = $("."+obj.id+" .buttonCenago.active").attr('section');
                              if (section!=active) {
                                        $("."+obj.id+" .buttonCenago.active").removeClass('active');
                                        $("."+obj.id+" div[block='"+active+"']").hide();
                                        $("."+obj.id+" div[section='"+section+"']").addClass('active');
                                        $("."+obj.id+" div[block='"+section+"']").show();
                              }
          },
          displayInformation:function(json){
                    var obj = this;
                    var chain = '<div class="title">'+obj.options.data.title+'</div>'+
                                '<div class="line"></div>'+
                                //'<div class="line2"></div>'+
                                '<div class="info">';
                                        obj.resetDataToDownload();
                                        for(var y in json.data.values.geographical){
                                                            var i = json.data.values.geographical[y];
                                                            chain+=obj.getItem(i.label,i.value,y);
                                        }
                                        for(var x in obj.additionalFields){
                                                  var i = obj.additionalFields[x];
                                                  var response = obj.findVariable(json.data.values.economical,x);
                                                  if (response.exist) {
                                                            var varSelected = i+obj.additionText[obj.options.data.typeSelected]
                                                            chain+=obj.getItem(varSelected,response.value,x);
                                                            obj.varSelected = 'Variable: '+varSelected;
                                                  }
                                        }
                                        var chainInfo='';
                                        var fistItem = null;
                                        chain+='<div class="chaska">';
                                        for(var x in obj.tree){
                                                  if (fistItem==null) {
                                                            fistItem=x+'';
                                                  }
                                                  var i = obj.tree[x];
                                                  chain+=obj.getButton(x);
                                                  chainInfo+=obj.getBlock(x,i,json.data.values.economical);
                                        }
                                        obj.dataToDownload.values.push(['']);
                                        obj.dataToDownload.values.push([obj.varSelected]);
                                        obj.dataToDownload.values.push([obj.textDownload[obj.options.data.typeSelected]]);
                                        chain+='</div>';
                                        chain+=chainInfo;
                                        
                      chain+= '</div>';
                    $("."+obj.id +" .container").html(chain);
                    $("."+obj.id+" .buttonCenago").click(function(){
                              var section = $(this).attr('section');
                              obj.showBlock(section);
                              
                    });
                    obj.showBlock(fistItem);
                    obj.hideSpinner();
          },
          getValueHeader:function(title,source){
                    var obj = this;
                    var response = title;
                    var result = '';
                    if (obj.valueHeader[title]) {
                        result = obj.findVariable(source,obj.valueHeader[title]);
                        response = title+': '+result.value;
                    }
                    return {label:response,value:result.value};
          },
          getBlock:function(source,data,json){
                    var obj = this;
                    var section = obj.getValueSection(source);
                    obj.dataToDownload.values.push([section]);
                    obj.dataToDownload.values.push(['']);
                    obj.dataToDownload.values.push(['Indicador','Hombres','Mujeres']);
                    var labelBlock = obj.getValueSection(source);
                    var chain='<div class="titleBlock">'+labelBlock+'</div>'+
                              '<div class="block" block="'+source+'">'+
                                        '<table>'+
                                                  '<thead>'+
                                                            '<tr class="titles">'+
                                                                      '<th>Indicador</th>'+
                                                                      '<th>Hombres</th>'+
                                                                      '<th>Mujeres</th>'+
                                                            '</tr>'+
                                                  '</thead>'+
                                                  '<tbody>';
                                                  for(var x in obj.tree[source]){
                                                            var item = obj.tree[source][x]+'';
                                                            var label = obj.getValueSection(item);
                                                            var hom='';
                                                            var muj='';
                                                            var responseHom = obj.findVariable(json,item.replace('__','_hom_'));
                                                            if (responseHom.exist) {
                                                                      hom=obj.getValue(responseHom.value);
                                                                      hom=obj.getFormatNumber(hom);
                                                            }
                                                            var responseMuj = obj.findVariable(json,item.replace('__','_muj_'));
                                                            if (responseMuj.exist) {
                                                                      muj=obj.getValue(responseMuj.value);
                                                                      muj=obj.getFormatNumber(muj);
                                                            }
                                                            if ((hom!='')||(muj!='')) {
                                                                      chain+='<tr>'+
                                                                                          '<td class="indicator">'+label+'</td>'+
                                                                                          '<td class="value">'+((hom)?hom:'')+'</td>'+
                                                                                          '<td class="value">'+((muj)?muj:'')+'</td>'+
                                                                             '</tr>';
                                                                      obj.dataToDownload.values.push([label,hom,muj]);
                                                            }
                                                            
                                                  }
                    chain+='</tbody></table></div>';
                    obj.dataToDownload.values.push(['']);
                    return chain;
          },
          getButton:function(i){
                    var obj = this;
                    var label = obj.getValueSection(i);
                    var chain='<div class="buttonCenago" section="'+i+'">'+label+'</div>';
                    //obj.dataToDownload.values.push([label,newValue]);
                    return chain;
          },
          getItem:function(label,value,variable){
                    var obj = this;
                    var classes = (variable==obj.options.data.variable)?'selected':'';
                    var newValue = obj.getValue(value);
			  		var chain='';
			  		if(label == 'Clave'){
						chain='<span class="text '+classes+'">'+'<div class="label">'+label+': <div class="valueText">&nbsp;'+newValue+'</div></div></span>';
					}else{
						chain='<span class="text '+classes+'">'+'<div class="label">'+label+': <div class="valueText">&nbsp;'+(obj.getFormatNumber(newValue))+'</div></div></span>';
					}
                    //var chain='<span class="text '+classes+'">'+'<div class="label">'+label+': <div class="valueText">&nbsp;'+(obj.getFormatNumber(newValue))+'</div></div></span>';
                    obj.dataToDownload.values.push([label,newValue]);
                    return chain;
          },
          resetDataToDownload:function(){
                    var obj = this;
                    obj.dataToDownload = null;
                    obj.dataToDownload = {title:obj.options.title,columns:[], values:[]};
          },
          getDataToDownload:function(data){
                    var obj=this;
                    var vals = obj.options.getGralValues();
                    var tempColumns=[];
                    var response={title:'',columns:[], values:[]};
                    response.columns.push(headerTable);
                    tempColumns.push(x);	 
                    
                   return response;
          },
          download:function(){
                    var obj= this;
                    var params = obj.dataToDownload;
                    var c = obj.options.data.connection;
                    var o=c.getXLS;/* var o=c.exportData;*/
                    var a=c.getXLS;
                    var request = {
                        type: o.type,
                        dataType: o.dataType,
                        url: o.url,
                        data:((o.stringify)?JSON.stringify(params):params),
                        contentType:o.contentType,
                        success:function(json,estatus){
                          var error=false;
                          if(json.response.success){
                                  $('#'+obj.idForm).remove();
                                  var cadena = '<form id="'+obj.idForm+'" action="'+a.urlGet+'/xls/'+json.data.id+'" method="GET" enctype="multipart/form-data" style="display:none">'+
                                                      '<input type="submit" value="Submit">'+
                                              '</form>';
                                  $("body").append(cadena);
                                  $('#'+obj.idForm).submit();
                                  
                          }else{
                              error=true
                          }
                          if(error){
                              alert("no valido");
                          }
                        },
                        beforeSend: function(solicitudAJAX) {
                          
                        },
                        error: function(solicitudAJAX,errorDescripcion,errorExcepcion) {
                          
                        },
                        complete: function(solicitudAJAX,estatus) {
                        }
                     };
                     
                     $.ajax(request);
          },
		  events:function(){
						var obj = this;
                        $("."+obj.id +" .close").click(function(){
                             obj.hide();
                        });
                        $("."+obj.id +" .print").click(function(){
                             obj.print();
                        });
                        $("."+obj.id +" .download").click(function(){
                             obj.download();
                        });
                        obj.centerWindow();
                        $("."+obj.id).draggable({ cancel: ".info" });
		  },
          print:function(){
                    var title='prueba';
                    var html ='<div style="position:absolute;left:0%;width:100%;">'+
                                        '<center>'+
                                                  '<img src="js/core/ui/widgets/cardCenago/img/logo.jpg" style="width:20%">'+
                                        '</center>'+
                                        '<div class="PopupElementItem none custom_cardCenago">'+
                                                           $('.'+obj.id+' .info').html()+
                                        '</div>'+
                              '</div>';
                    printControl.printHtml(html);
          },
		  update:function(){
                    this.request(this.options.data.find,'find');
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
								case 'data':
											this.options.data=value;
											this.update();
								break;
						}
		  }
});
});