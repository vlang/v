const messageList = document.getElementById('message-list');
const protocol = location.protocol === 'https:' ? 'wss' : 'ws';
const socket = new WebSocket(`${protocol}://${location.host}/ws`);
let i = 0;

function send(message) {
  messageList.innerHTML += `<li>&gt; ${message}</li>`;
  socket.send(message);
}

socket.addEventListener("open", (event) => {
  console.log('Connected to WS server');
  send('Hey everyone !');
});

socket.addEventListener("message", (event) => {
  const { data } = event;
  messageList.innerHTML += `<li>&lt; ${data}</li>`;
  setTimeout(() => {
    send(`Roger ${i++}`);
  }, 3000);
});