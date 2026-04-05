/** LIKES */

// Callback function
function show(name) {
	// Get output container
	var result = document.getElementById("result");
	// Return callback function
	return function(answer) {
		// Valid answer
		if(pl.type.is_substitution(answer)) {
			// Get the value of the food
			var food = answer.lookup("X");
			// Get the person
			var person = name != "Y" ? name : answer.lookup("Y");
			// Show answer
			result.innerHTML = result.innerHTML + "<div>" + person + " likes " + food + "</div>";
		}
	};
}

// Show the likes of a person
function likes(name) {
	// Create session
	var session = pl.create(1000);
	// Get program
	var program = document.getElementById("program").value;
	// Clear output
	document.getElementById("result").innerHTML = "";
	// Consult program
	session.consult(program);
	// Query goal
	session.query("likes(" + name + ", X).");
	// Find answers
	session.answers(show(name), 1000);
}

/** EVENTS */

// onClick #button
function clickButton() {
	// Get person
	var name = document.getElementById("name").value;
	name = name != "" ? name : "Y";
	// Get likes
	likes(name);
}

// onChange #name
function changeName() {
	var name = document.getElementById("name").value;
	document.getElementById("button").value = name != "" ? "What does " + name  + " like?" : "See all likes";
}
changeName();


