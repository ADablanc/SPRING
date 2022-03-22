$(document).on("shiny:connected", function(e) {
	$("body").addClass("sidebar-mini");
	$(".dropdown-menu").css("width", window.innerWidth/4.8);
});

$(document).on("click", "#conflicts_table button", function() {
    Shiny.onInputChange(
        "conflicts_table_valid",
        {
            bttn: Math.random(),
            value: $(this).attr("value")
        }
    );
});
