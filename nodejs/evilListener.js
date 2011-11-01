var http = require('http');
var url  = require('url');

var posts = [];

var header = '\
<html>\
  <head>\
    <title>Evil Listener</title>\
    <script src="http://ajax.googleapis.com/ajax/libs/jquery/1.6.4/jquery.min.js"></script>\
	 <style type="text/css">\
	   .time { color:grey;  }\
	 </style>\
    <script type="text/javascript">\
      $(document).ready(function () {\
         $("#clear").click(function() {\
           $.post("clear");\
           $("#msgs").html("");\
         });\
      });\
    </script>\
  </head>\
\
  <body style="font-size:2em;margin-left:auto;margin-right:auto;width:640px;">\
    <h2 style="text-align:center;">Evil Listener</h2>\
    <input id="clear" type="submit" value="Clear Log"/>\
    <p id="msgs" style="margin-left:auto;margin-right:auto;width:640px;">';

var ender = '\
    </p>\
  </body>\
</html>';

http.createServer(function (req, res) {
   get = url.parse(req.url, true);

   // handle clear
   if (get.pathname === '//clear') {
      posts = [];
		console.log('Clear');
   // handle post message
   } else if (get.query != null && get.query.msg != null) {
      posts.unshift(getTime() + get.query.msg);
      console.log('Got message: ' + get.query.msg);
   }

   res.writeHead(200, {'Content-Type': 'text/html'});
   var s = "";
   for (p in posts) {
      s += posts[p] + '<br>';
   }
   res.end(header + s + '\n' + ender);
}).listen(1337, "127.0.0.1");

console.log('Server running at http://127.0.0.1:1337/');

function getTime() {
	var x = new Date();
	var h = x.getHours();
	var m = x.getMinutes();
	var s = x.getSeconds();
	h = ((h < 10) ? '0' : '') + h;
	m = ((m < 10) ? '0' : '') + m;
	s = ((s < 10) ? '0' : '') + s;
	return ('<span class="time">[' + h + ':' + m + ':' + s + ']</span> ');
}

