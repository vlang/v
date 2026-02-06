const messageList = document.getElementById('message-list');
const messageInput = document.getElementById('message-input');
const messageStatus = document.getElementById('message-status');
const protocol = location.protocol === 'https:' ? 'wss' : 'ws';
//
let status = '---';
function set_status(s) {  status = s; }
function show_status() {
   messageStatus.innerHTML = `${status}, ${socket.readyState}`;
   requestAnimationFrame(show_status);
}						 
requestAnimationFrame(show_status);
//
var socket = start_reconnecting_socket(1000, `${protocol}://${location.host}/ws`);
function start_reconnecting_socket(timeout, target) {
   var nsocket = new WebSocket(target);
   nsocket.addEventListener('open', (event) => {   console.log('Connected to WS server'); set_status('connected'); });
   nsocket.addEventListener('message', (event) => {
      messageList.innerHTML += `<li>received: <b>${event.data}</b></li>`;
      set_status('received message');
   });
   nsocket.addEventListener('close', (event) => {  
	  console.log('on close'); 
	  set_status('connection closed');
	  setTimeout(()=>{
	     console.log('Try reconnecting ...')
		 socket = start_reconnecting_socket(timeout, target);
	  }, timeout);
   });
   nsocket.addEventListener('error', (event) => {  console.log('on error'); set_status('error'); });
   return nsocket;
}

//
function send(message) {
  socket.send(message);
  set_status('sending message');
}
messageInput.addEventListener('keyup', ({key}) => {
   if (key === 'Enter') {
	  send(messageInput.value);
	  messageInput.value = '';
   }
});
