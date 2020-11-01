sap.ui.define(["Mosbach/StudiHub/controller/base.controller","sap/m/UploadCollectionParameter","sap/m/MessageToast"],function(e,t,a){"use strict";return e.extend("Mosbach.StudiHub.controller.Offer.CreateOffer",{extendInit:function(){this.onLiveChange();t+
his.oRouter.getRoute("RouteCreateOffer").attachPatternMatched(this._onObjectMatched,this)},_onObjectMatched:function(){this.byId("panelData").bindElement("/UserSet('"+this.oSession.getData().myUserId+"')");this.byId("inputHeader").setValue("");this.byId(+
"inputShortHeader").setValue("");this.byId("inputNumber").setValue("");this.byId("textArea").setValue("");this.byId("selectIcon").setValue("");this.byId("dateRange").setDateValue();this.byId("dateRange").setSecondDateValue();this.byId("UploadCollection")+
.removeAllItems();this.xBinary=""},onLiveChange:function(){var e=this.byId("inputShortHeader").getValue();var t=this.byId("inputCompany").getValue();var a=this.byId("selectIcon").getSelectedKey();var i=this.byId("inputNumber").getValue();if(this.byId("da+
teRange").getSecondDateValue()){var s="Bis: "+this.dateFormatter(this.byId("dateRange").getSecondDateValue())}this.generateOfferTile(e,t,a,i,s)},pressNavBack:function(){var e=sap.ui.core.UIComponent.getRouterFor(this);e.navTo("RouteMasterView")},generate+
OfferTile:function(e,t,a,i,s){var r=new sap.m.GenericTile;var n=new sap.m.TileContent;var o=this.getTileContent(i,a);r.setHeader(e);r.setSubheader(t);r.setMode();r.addStyleClass("sapUiTinyMargin");r.addStyleClass("sapUiTinyMargin");n.setFooter(s);n.setCo+
ntent(o);r.addTileContent(n);this.byId("flexBoxPreview").destroyItems();this.byId("flexBoxPreview").addItem(r)},getTileContent:function(e,t){var a;if(e){a=new sap.m.NumericContent;a.setValue(e);a.setIcon(t);a.setWithMargin(false)}else{a=new sap.m.ImageCo+
ntent;a.setSrc(t)}return a},getOffer:function(){return{Postid:"0",Userid:this.oSession.getData().myUserId,Begda:this.removeTimeZoneOffset(this.byId("dateRange").getDateValue()),Endda:this.removeTimeZoneOffset(this.byId("dateRange").getSecondDateValue()),+
Title:this.byId("inputHeader").getValue(),ShortTitle:this.byId("inputShortHeader").getValue(),OfferValue:this.byId("inputNumber").getValue(),Icon:this.byId("selectIcon").getSelectedKey(),Street:this.byId("inputStreet").getValue(),City:this.byId("inputCit+
y").getValue(),Postcode:this.byId("inputPostcode").getValue(),Descr:this.byId("textArea").getValue(),Weblink:this.byId("inputWebsite").getValue(),Pdfsource:"",Pdfbinary:this.xBinary,Uname:""}},onSavePress:function(){var e=this.getOffer();if(this.fieldsVa+
lid(e)){this.saveOffer(e)}},fieldsValid:function(e){var t=false;if(!e.Title){this.byId("inputHeader").setValueState("Error");t=true}else{this.byId("inputHeader").setValueState("None")}if(!e.ShortTitle){this.byId("inputShortHeader").setValueState("Error")+
;t=true}else{this.byId("inputShortHeader").setValueState("None")}if(!e.Begda||!e.Endda){this.byId("dateRange").setValueState("Error");t=true}else{this.byId("dateRange").setValueState("None")}if(!e.Descr){this.byId("textArea").setValueState("Error");t=tru+
e}else{this.byId("textArea").setValueState("None")}if(t){a.show("Bitte fülle alle Pflichtfelder aus!")}return!t},saveOffer:function(e){var t=this;var i;this.oModel.create("/OfferSet",e,{success:function(e,s){a.show("Angebot erfolgreich angelegt!");i=e.Po+
stid;setTimeout(function(){t.oRouter.navTo("RouteDetailOffer",{ID:"O"+i})},1500)},error:function(){a.show("Fehler beim Speichern des Angebots!")}})},onFileSelected:function(e){var a=e.getSource();a.removeAllItems();var i=new t({name:"x-csrf-token",value:+
this.oModel.getSecurityToken()});a.addHeaderParameter(i);this.saveFileTemporary(e.getParameter("files")[0])},saveFileTemporary:function(e){var t;var a=new FileReader;var i=this;a.onload=function(e){var a=e.target.result;var s;if(btoa(a)){s=btoa(a)}else{s+
=btoa(encodeURIComponent(a))}if(typeof base64file==="undefined"||typeof base64file===null){t=s}else{t=t+"new"+s}i.xBinary=t};a.readAsBinaryString(e)}})});                                                                                                     