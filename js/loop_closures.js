// Testing closure variable capture semantics of JS.
// Free variables are captured by reference, so need to be very careful
// (especially in loops). Solution (if want to capture the value of a variable
// when the closure is defined) is to convert the free variable to an argument.
// Also a good idea to move the closure / function definition outside the loop
// for performance reasons (avoids creating a new function object for each loop
// iteration).
//

function use_cb(i, cb) {
    console.log(i + ' -> doing something...');
    setTimeout(function () { cb(i) }, 4000 - i*1000);
}

function test() {
    var servers = ['server1.com', 'server2.com'];
    for (var i in servers) {
        console.log(i + ' -> ' + servers[i]);
        use_cb(i, function(j) {
            console.log(i + ' -> done!'); // capture by reference
            // VS.
            // console.log(j + ' -> done!'); // capture by value
        });
    }
}

test();
