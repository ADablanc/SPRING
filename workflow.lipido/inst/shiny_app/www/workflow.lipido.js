$(document).on("shiny:connected", function(e) {
	$("body").addClass("sidebar-mini");
	$('.dropdown-menu').css('width', window.innerWidth/4.8);
	show_project_dialog()
});

function show_project_dialog() {
    swal.fire({
	 html: '<button id = "project_load" class="swal2-confirm swal2-styled"> Load project file </button> <hr /><button id = "project_create" class="swal2-cancel swal2-styled"> Create new project </button>',
        showCancelButton: false,
        showConfirmButton: false,
		allowOutsideClick: false,
		allowEscapeKey: false
	});
}
$(document).on("click", "#project_load", function() {
    $("#project_load2").click();
});

$(document).on("click", "#project_create", function() {
    show_create_project_dialog();
});

function show_create_project_dialog() {
    swal.fire({
    title: "Create project",
	 html: '<label for="project_create_name" class="swal2-input-label">The name of the project</label><input class="swal2-input" style="display: flex;" id="project_create_name" placeholder="" type="text"><div class="form-group" style = "border: solid"><button id = "project_create_path">Choose directory</button><pre class = "shiny-text-output noplaceholder" id="project_create_path_display">No direcory selected</pre></div>',
	    showCancelButton: true,
        showConfirmButton: true,
        allowOutsideClick: false,
		allowEscapeKey: false
	}).then((result) =>  {
	    if (result.isConfirmed) {
	        debugger;
	        Shiny.onInputChange("project_create", {
	            fire: Math.random(),
	            project_name: $("#project_create_name").val()
	        });
	    } else {
	        show_project_dialog();
	    }
	});
}
$(document).on("click", "#project_create_path", function(e) {
    e.preventDefault();
    $("#project_create_path2").click();
});
function load_db(val = "") {
    $("#project_name").text(val);
    swal.close();
}

$(document).on("click", "#conflicts_table button", function() {
    Shiny.onInputChange(
        "conflicts_table_valid",
        {
            bttn: Math.random(),
            value: $(this).attr("value")
        }
    );
});
