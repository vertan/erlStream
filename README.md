#erlStream

erlStream is a system for streaming audio from a server to clients.

The server is written in Erlang and the Client in Java.

## Cloning the repo
```bash
git clone https://github.com/Vertig0/erlStream.git
```

## Compiling the system
```bash
cd path/to/erlStream
make
```

Note: You can compile the server or the client separately with ```make server``` and ```make client``` respectively.

## Running the system (Local server)

#### Terminal 1 (Server)
```bash
cd path/to/erlStream
make start_server
```

#### Terminal X (Client X-1)
```bash
cd path/to/erlStream
make start_client
```

## Running the system (Remote server)

#### Terminal 1 (Server)
```bash
ssh username@yourserver.com
git clone https://github.com/Vertig0/erlStream.git
cd erlStream
make start_server
```

#### Terminal X (Client X-1)
```bash
cd path/to/erlStream
make
cd client/bin
java Client yourserver.com
```
