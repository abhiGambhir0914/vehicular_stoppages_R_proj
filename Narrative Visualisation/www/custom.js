$(document).ready(function(){

	
	// $('#animated_time_slider').hide()
	
	$('#my_button2').hide()
	// $('.play').hide()
	// $("#animated_time_slider").hide()

	$('#my_button1').click(function(){
		$('#my_button1').hide()
		$('#my_button2').show()
		$('.play').hide()
		$('.pause').hide()
		// $("#animated_time_slider").show()
		setTimeout(function() {$('.slider-animate-button').click()},10);
	});

	$('#my_button2').click(function(){
		$('#my_button2').hide()
		$('#my_button1').show()
		// $("#animated_time_slider").hide()
		// $('.slider-animate-button').click()
		$('.slider-animate-button').click()
	});

	$('.slider-animate-button').click(function(){
		$('#my_button1').hide()
		$('#my_button2').show()
	})

});