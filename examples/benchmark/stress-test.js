var net = require('net');
socket = net.connect(7000);
socket.on('connect', function() {
  for(i = 1; i <= 1000000; i++) {
    socket.write(
      "DE000" + (1000000 + i) + ";[data]\n"
    );
  }
  socket.end();
});