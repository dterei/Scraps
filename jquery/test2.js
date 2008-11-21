$(document).ready(function(){
	jQuery.fn.fadeToggle = function(speed, easing, callback) {
		return this.animate({opacity: 'toggle'}, speed, easing, callback);
	}; 

	$(".res-hstat").hide();

	$(".res-show").click(function(event){
		$(".res-hstat[id=res-" + $(this).attr("id") + "]").fadeToggle();
		if ($(this).text() == ">") {
			$(this).text("<");
		} else {
			$(this).text(">");
		}
	});
});
