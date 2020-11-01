sap.ui.define(["Mosbach/StudiHub/controller/base.controller","sap/m/MessageToast","sap/m/UploadCollectionParameter"],function(e,t,a){"use strict";return e.extend("Mosbach.StudiHub.controller.Job.CreateJob",{extendInit:function(){this.onLiveChange();this.+
oRouter.getRoute("RouteCreateJob").attachPatternMatched(this._onObjectMatched,this)},_onObjectMatched:function(){this.byId("panelData").bindElement("/UserSet('"+this.oSession.getData().myUserId+"')");this.byId("inputHeader").setValue("");this.byId("input+
ShortHeader").setValue("");this.byId("inputNumber").setValue("");this.byId("textArea").setValue("");this.byId("dateRange").setDateValue();this.byId("dateRange").setSecondDateValue();this.byId("UploadCollection").removeAllItems();this.xBinary=""},onLiveCh+
ange:function(){var e=this.byId("inputShortHeader").getValue();var t=this.byId("inputCompany").getValue();var a=this.byId("inputNumber").getValue()+"/Std";if(this.byId("dateRange").getSecondDateValue()){var i="Bis: "+this.dateFormatter(this.byId("dateRan+
ge").getSecondDateValue())}this.generateJobTile(e,t,a,i)},pressNavBack:function(){this.oRouter.navTo("RouteMasterView")},generateJobTile:function(e,t,a,i){var s=new sap.m.GenericTile;var r=new sap.m.TileContent;var n=new sap.m.NumericContent;s.setHeader(+
e);s.setSubheader(t);s.addStyleClass("sapUiTinyMarginBegin");s.addStyleClass("sapUiTinyMarginTop");s.addStyleClass("tileLayout");r.setFooter(i);n.setValue(a);r.setContent(n);s.addTileContent(r);this.byId("flexBoxPreview").destroyItems();this.byId("flexBo+
xPreview").addItem(s)},getJob:function(){var e={Postid:"0",Userid:this.oSession.getData().myUserId,Begda:this.removeTimeZoneOffset(this.byId("dateRange").getDateValue()),Endda:this.removeTimeZoneOffset(this.byId("dateRange").getSecondDateValue()),Title:t+
his.byId("inputHeader").getValue(),ShortTitle:this.byId("inputShortHeader").getValue(),Wage:this.byId("inputNumber").getValue()+"",Street:this.byId("inputStreet").getValue(),City:this.byId("inputCity").getValue(),Postcode:this.byId("inputPostcode").getVa+
lue(),Descr:this.byId("textArea").getValue(),Weblink:this.byId("inputWebsite").getValue(),Uname:"",Pdfsource:"",Pdfbinary:this.xBinary};if(!e.Wage)e.Wage="0";return e},onSavePress:function(){var e=this.getJob();if(this.fieldsValid(e)){this.saveOffer(e)}}+
,fieldsValid:function(e){var a=false;if(!e.Title){this.byId("inputHeader").setValueState("Error");a=true}else{this.byId("inputHeader").setValueState("None")}if(!e.ShortTitle){this.byId("inputShortHeader").setValueState("Error");a=true}else{this.byId("inp+
utShortHeader").setValueState("None")}if(!e.Begda||!e.Endda){this.byId("dateRange").setValueState("Error");a=true}else{this.byId("dateRange").setValueState("None")}if(!e.Descr){this.byId("textArea").setValueState("Error");a=true}else{this.byId("textArea"+
).setValueState("None")}if(a){t.show("Bitte fülle alle Pflichtfelder aus!")}return!a},saveOffer:function(e){var a=this;var i;this.oModel.create("/JobSet",e,{success:function(e,s){t.show("Job erfolgreich angelegt!");i=e.Postid;setTimeout(function(){a.oRou+
ter.navTo("RouteDetailJob",{ID:"J"+i})},1500)},error:function(){t.show("Fehler beim Speichern des Posts!")}})},onFileSelected:function(e){var t=e.getSource();t.removeAllItems();var i=new a({name:"x-csrf-token",value:this.oModel.getSecurityToken()});t.add+
HeaderParameter(i);this.saveFileTemporary(e.getParameter("files")[0])},saveFileTemporary:function(e){var t;var a=new FileReader;var i=this;a.onload=function(e){var a=e.target.result;var s;if(btoa(a)){s=btoa(a)}else{s=btoa(encodeURIComponent(a))}if(typeof+
 base64file==="undefined"||typeof base64file===null){t=s}else{t=t+"new"+s}i.xBinary=t};a.readAsBinaryString(e)}})});                                                                                                                                           