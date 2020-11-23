# mongov

## Install dependencies **Ubuntu 18.04 & 20.04**:

    sudo apt-get install cmake libssl-dev libsasl2-dev &&
    wget https://github.com/mongodb/mongo-c-driver/releases/download/1.16.2/mongo-c-driver-1.16.2.tar.gz && # Check for the latest version
    tar xzf mongo-c-driver-1.16.2.tar.gz &&
    cd mongo-c-driver-1.16.2 &&
    mkdir cmake-build
    cd cmake-build
    cmake -DENABLE_AUTOMATIC_INIT_AND_CLEANUP=OFF .. &&
    make &&
    sudo make install

## Start mongoDB instance:

    docker-compose up