
function setvalues(){
  getidinfo();
}

var inputIdInfoBinding = new Shiny.InputBinding();
$.extend(inputIdInfoBinding, {
  find: function(scope) {
    return $.find('.idinfo');
  },
  getValue: function(el) {
    return $(el).val();
  },
  setValue: function(el, values) {
    $(el).attr("value", getidinfo());
    $(el).trigger("change");
  },
  subscribe: function(el, callback) {
    $(el).on("change.inputIdInfoBinding", function(e) {
      callback();
    });
  },
  unsubscribe: function(el) {
    $(el).off(".inputIdInfoBinding");
  }
});
Shiny.inputBindings.register(inputIdInfoBinding);


function getidinfo() {
  var obj = [];
    $.getJSON("http://jsonip.com/?callback",
        function(data){
           console.log( "success" );
           obj.push(data.ip);
           callback(obj);
           $(".idinfo").attr("value", obj.join("|"));
           $(".idinfo").trigger("change");
    }).fail(function() { 
          console.log( "error" ); 
          var ip = '123';
          callback(ip);
          $(".idinfo").attr("value", ip);
          $(".idinfo").trigger("change");
    });
}

function callback(tempobj)
{
obj=tempobj;
}
