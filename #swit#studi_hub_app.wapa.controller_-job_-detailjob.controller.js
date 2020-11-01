sap.ui.define(["Mosbach/StudiHub/controller/base.controller","Mosbach/StudiHub/assets/js/maps/MapsClass","sap/ui/model/json/JSONModel","sap/ui/Device"],function(e,t,s,o){"use strict";return e.extend("Mosbach.StudiHub.controller.Job.DetailJob",{extendInit+
:function(){this.oRouter.getRoute("RouteDetailJob").attachPatternMatched(this._onObjectMatched,this);this.oMaps=new t(this.getView().byId(this.getVisibleMapId()));this.setDeviceModel()},_onObjectMatched:function(e){var t=this;this.postId=e.getParameter("+
arguments").ID;var s="/JobSet('"+this.postId.substr(1)+"')";var o;this.oModel.read(s,{success:function(e){o=e.Postcode+", "+e.City+", "+e.Street;t.oMaps.setLocation(e.Postcode+", "+e.City+", "+e.Street);t.byId("userProfileButton").bindElement("/UserSet('+
"+e.Userid+"')")}});this.loadComments();this.getView().bindElement(s)},getVisibleMapId:function(){if(sap.ui.Device.system.desktop===true){return"vbi"}else{return"vbiMobile"}},setDeviceModel:function(){var e=new s(o);e.setDefaultBindingMode("OneWay");this+
.getView().setModel(e,"device")},loadComments:function(){var e=new sap.m.FeedListItem({sender:"{Uname}",icon:"",senderPress:"onSenderPress",iconPress:"onIconPress",iconDensityAware:false,info:"",timestamp:"{Createddate}, {Createdtime}",text:"{Text}",conv+
ertLinksToAnchorTags:"All"});this.byId("commentList").bindAggregation("items",{path:"/CommentSet",template:e,filters:[new sap.ui.model.Filter("Postid",sap.ui.model.FilterOperator.EQ,this.postId.substr(1))]})},onEditJobPress:function(){this.oRouter.navTo(+
"RouteEditJob",{ID:this.postId})},onProfilePress:function(e){var t=e.getSource().getBindingContext().getObject();if(t.Usertype!=="STDT"){this.navToProfile(t.Usertype.substr(0,1)+t.Userid)}},onWeblinkPress:function(e){var t=e.getSource().getTooltip();if(t+
.substr(4)!=="http")t="https://"+t;window.open(t,"_blank")},onComment:function(e){var t={Postid:this.postId.substr(1),Userid:this.oSession.getData().myUserId,Createddate:"",Createdtime:"",Text:e.getSource().getValue(),Uname:"",Usertype:""};this.oModel.cr+
eate("/CommentSet",t,{success:function(e,t){},error:function(e){}})}})});                                                                                                                                                                                      