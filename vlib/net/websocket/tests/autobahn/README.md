# Autobahn tests

This is the autobahn automatic tests on build.
The performance tests are skipped due to timeouts in Github actions.

## Run it locally

### Test the client

This is how to test the client:

1. Run the docker autobahn test suite by running the `docker-compose up`
2. From the `local_run` folder, compile and run `autobahn_client.v` to test non ws (no TLS) and 
`autobahn_client_wss.v` to run the TLS tests
3. Open `http://localhost:8080` and browse client test results for non TLS and `https://localhost:8081` 
if you ran the wss tests (it uses local certificat so you will get trust error but just accept use)

### Test the server

Todo: add information here