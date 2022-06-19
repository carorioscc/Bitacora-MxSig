$.widget( "custom.Popup", {
				id:'customPopup',
				padding:30,
				heightRow:25,
				heightRow2:20,
				widthRow:30,
				positionRow:35,
				borderPopup:4,
                options:{
                    viewOrder:['top-left','top-right','bottom-left','bottom-right','right-top','right-bottom','left-top','left-bottom'],
					root:'body',
					divLimiter:'#map',
					showOn:'now',//'click','now',
					time:null,
					iconClose:true,
					highlight:false,
					events:{
						onShow:$.noop,
						onHide:$.noop,
						onCreate:$.noop
					},
					content:'Sin contenido',
					created:false
                },
				_create: function() {
					var obj = this;
					var o = obj.options;
					if (!o.created) {
						obj.buildCloud();
						obj.defineProperties();
						obj.events();
						obj.options.created=true;
						o.created=true;
					}
                },
				buildCloud:function(){
					var obj = this;
					var parentId = obj.element.attr('id');
					obj._id = obj.id+'_'+parentId;
					
					var iconClose = (obj.options.iconClose)?'<span class="close dinamicPanel-sprite dinamicPanel-close-short"></span>':'';
					var chain=	'<div id="'+obj._id+'" class="'+obj.id+'">'+iconClose+'<div class="content">'+obj.options.content+'</div></div>';
					$(obj.options.root).append(chain);
				},
				updateContent:function(content){
					var obj = this;
					obj.options.content = content;
					$("#"+obj._id+" .content").html(content);
					obj.defineProperties();
					if (obj.options.showOn=='now') {
                        obj.show();
                    }
				},
				defineProperties:function(){
					var obj = this;
					var o = obj.options;
					var item = this.element;
					var position = item.offset();
					obj._height = item.height();
					obj._width = item.width();
					obj._widthContent = $("#"+obj._id).width()+obj.padding;
					obj._heightContent = $("#"+obj._id).height()+obj.padding;
					obj._xy = position;
					obj.setLimits();
					for(var x in o.viewOrder){
						var response = obj.isValid(o.viewOrder[x]);
						if (response.valid) {
							break;
                        }
					}
					$("#"+obj._id).css({left:response.left+'px',top:response.top+'px'}).addClass(response.position);
				},
				setLimits:function(){
					var obj = this;
					obj.limits = {left:0,right:0,top:0,bottom:0};
					obj.limits.right=$(obj.options.divLimiter).width();
					var position = $(obj.options.divLimiter).offset();
					obj.limits.top=position.top;
					obj.limits.left=position.left;
					obj.limits.bottom=$(obj.options.divLimiter).height();
				},
				isValid:function(pos){
					var obj = this;
					var r= {top:0,left:0,valid:false,position:pos};
					var wp,hp,lp,tp;
					switch (pos) {
                        case 'top-left':
							r.top = obj._xy.top-(obj._heightContent+obj.heightRow);
							r.left = (obj._xy.left+(obj._width/2))-obj.positionRow;
							
							wp=obj._widthContent+obj.borderPopup;
							hp=obj._heightContent+obj.heightRow;
							tp=r.top;
							lp=r.left;
							
							break;
						case 'top-right':
							r.top = obj._xy.top-(obj._heightContent+obj.heightRow);
							r.left = (obj._xy.left+(obj._width/2))-(obj._widthContent-obj.positionRow);
							
							wp=obj._widthContent+obj.borderPopup;
							hp=obj._heightContent+obj.heightRow;
							tp=r.top;
							lp=r.left;
							
							break;
						case 'bottom-left':
							r.top = obj._xy.top+(obj._height+obj.heightRow);
							r.left = (obj._xy.left+(obj._width/2))-obj.positionRow;
							
							wp=obj._widthContent+obj.borderPopup;
							hp=obj._heightContent+obj.heightRow;
							tp=r.top-obj.heightRow2;
							lp=r.left;
							
							break;
						case 'bottom-right':
							r.top = obj._xy.top+(obj._height+obj.heightRow);
							r.left = (obj._xy.left+(obj._width/2))-(obj._widthContent-obj.positionRow);
							
							wp=obj._widthContent+obj.borderPopup;
							hp=obj._heightContent+obj.heightRow;
							tp=r.top-obj.heightRow2;
							lp=r.left;
							
							break;
						case 'right-top':
							r.top = (obj._xy.top+(obj._height/2))-obj.heightRow;
							r.left = (obj._xy.left+obj._width);
							
							wp=obj._widthContent+obj.widthRow+obj.borderPopup;
							hp=obj._heightContent+obj.borderPopup;
							tp=r.top;
							lp=r.left;
							
							break;
						case 'right-bottom':
							r.top = (obj._xy.top+obj._height/2)-(obj._heightContent-obj.heightRow);
							r.left = (obj._xy.left+obj._width);
							
							wp=obj._widthContent+obj.widthRow+obj.borderPopup;
							hp=obj._heightContent+obj.borderPopup;
							tp=r.top;
							lp=r.left;
							
							break;
						case 'left-top':
							r.top = (obj._xy.top+(obj._height/2))-obj.heightRow;
							r.left = (obj._xy.left-obj._widthContent)-obj.heightRow;
							
							wp=obj._widthContent+obj.widthRow+obj.borderPopup;
							hp=obj._heightContent+obj.borderPopup;
							tp=r.top;
							lp=r.left;
							
							break;
						case 'left-bottom':
							r.top = (obj._xy.top+obj._height/2)-(obj._heightContent-obj.heightRow);
							r.left = (obj._xy.left-obj._widthContent)-obj.heightRow;
							
							wp=obj._widthContent+obj.widthRow+obj.borderPopup;
							hp=obj._heightContent+obj.borderPopup;
							tp=r.top;
							lp=r.left;
							
							break;
                    }
					r.valid = obj.insideLimit(wp,hp,tp,lp);
					//$(".pruebapopup").remove();
					//var chain = '<div class="pruebapopup" style="width:'+wp+'px;height:'+hp+'px;left:'+lp+'px;top:'+tp+'px"></div>';
					//$(obj.options.root).append(chain);
					return r;
				},
				insideLimit:function(width,height,top,left){
					var obj=this;
					var response = false;
					if ((left>=obj.limits.left)&&(top>=obj.limits.top)&&(width<=obj.limits.right)&&(height<=obj.limits.bottom)) {
                        response=true;
                    }
					return response;
				},
				show:function(){
					var obj = this;
					$("#"+obj._id).fadeIn();
					if (obj.options.highlight) {
                        $("#"+obj._id).removeClass('Blinker').addClass('Blinker');
						
                    }
					obj.options.events.onShow();
					if (obj.options.time) {
                        setTimeout(function(){
							obj.hide();
						},obj.options.time);
                    }
				},
				hide:function(){
					var obj = this;
					$("#"+obj._id).fadeOut();
					if (obj.options.highlight) {
						$("#"+obj._id+' .content').removeClass('Blinker');
                    }
					obj.options.events.onHide();
				},
				events:function(){
					var obj=this;
					if (obj.options.iconClose) {
                        $("#"+obj._id + " .close").click(function(){
							obj.hide();
						});
                    }
					switch (obj.options.showOn){ 
                        case 'now':
								obj.show();
							break;
						case 'onclick':
								$("#"+obj._id).click(function(){
									obj.show();
								});
							break;
						case 'over':
							    var parentId = obj.element.attr('id');
								$("#"+parentId).mouseenter(function(){
									obj.show();
								});
								$("#"+parentId).mouseleave(function(){
									obj.hide();
								});
							break;
                    }
					obj.options.events.onCreate();
				},
                _refresh: function(){
                  // trigger a callback/event
                  this._trigger( "change" );
                },
               
                _destroy: function() {
                    this.element.remove();
                },
                _setOption: function(key, value){
                          this.options[key] = value;
                                    switch(key){
                                            case "content":
                                                    this.updateContent(value);
                                                break;         
                                    }
                          }
                }
    );