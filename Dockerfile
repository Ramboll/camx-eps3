ARG ONEAPI_VERSION="oneapi-hpckit:2025.3.0-0-devel-ubuntu24.04"
ARG EPS3_VERSION="3.40"
ARG NETCDF_VERSION="4.4.4"
ARG NETCDFC_VERSION="4.4.1.1"
ARG HDF5_VERSION="1.14.6"
ARG ZLIB_VERSION="1.3.1"
ARG MAKE_OPTS="-j4"

# Stage 1: Build libraries for CAMx -- zlib, hdf5, and netCDF
FROM docker.io/intel/${ONEAPI_VERSION} AS netcdf-builder
ARG ZLIB_VERSION
ARG HDF5_VERSION
ARG NETCDF_VERSION
ARG NETCDFC_VERSION
ARG MAKE_OPTS

ENV DIR="/usr/local" \
    CC=icx

# Update package list and install dependencies
RUN apt-get update && \
    apt-get install -y \
        wget \
        build-essential \
        gfortran \
        libhwloc-dev \
        libxml2-dev \
        libnuma-dev \
        libx11-dev \
        libxt-dev \
        libevent-dev \
        libfabric-dev \
        libucx-dev \
        libibverbs-dev \
        librdmacm-dev \
        libpng-dev \
        vim \
        csh \
        tcsh \
        file \
        ca-certificates

# Install zlib
RUN cd /tmp && \
    wget https://github.com/madler/zlib/releases/download/v${ZLIB_VERSION}/zlib-${ZLIB_VERSION}.tar.gz && \
    tar -xzf zlib-${ZLIB_VERSION}.tar.gz && \
    cd zlib-${ZLIB_VERSION} && \
    ./configure --prefix=$DIR/zlib-${ZLIB_VERSION} && \
    make ${MAKE_OPTS} && \
    make install && \
    cd /usr/local && \
    ln -sf zlib-${ZLIB_VERSION} zlib && \
    cd / && \
    rm -rf /tmp/zlib-${ZLIB_VERSION}*

# Install HDF5 without parallel support
RUN cd /tmp && \
    wget https://github.com/HDFGroup/hdf5/archive/refs/tags/hdf5_${HDF5_VERSION}.tar.gz && \
    tar -xzf hdf5_${HDF5_VERSION}.tar.gz && \
    cd hdf5-hdf5_${HDF5_VERSION} && \
    ./configure \
        --prefix=/usr/local/hdf5-${HDF5_VERSION} \
        --enable-shared \
        --enable-hl \
        --enable-fortran \
        --with-zlib=/usr/local/zlib-${ZLIB_VERSION} \
        --with-default-api-version=v110 \
    CC=icx \
    FC=ifx \
    CFLAGS="-O2 -fPIC" \
    FFLAGS="-O2 -fPIC" \
    LDFLAGS="-L/opt/intel/oneapi/compiler/latest/lib"

RUN cd /tmp/hdf5-hdf5_${HDF5_VERSION} && \
    make ${MAKE_OPTS} && \
    make install && \
    cd / && \
    rm -rf /tmp/hdf5-hdf5-${HDF5_VERSION}* && \
    cd /usr/local && \
    ln -sf hdf5-${HDF5_VERSION} hdf5

# Install NetCDF-C with NetCDF4 support (no parallel I/O)
RUN cd /tmp && \
    wget https://github.com/Unidata/netcdf-c/archive/v${NETCDFC_VERSION}.tar.gz -O netcdf-c-${NETCDFC_VERSION}.tar.gz && \
    tar -xzf netcdf-c-${NETCDFC_VERSION}.tar.gz && \
    cd netcdf-c-${NETCDFC_VERSION} && \
    ./configure \
        --prefix=/usr/local/netcdf-${NETCDF_VERSION} \
        --enable-netcdf-4 \
        --enable-shared \
        --disable-parallel-tests \
        --disable-dap \
        CC=icx \
        CFLAGS="-O2 -fPIC -I/usr/local/hdf5-${HDF5_VERSION}/include -I/usr/local/zlib-${ZLIB_VERSION}/include" \
        LDFLAGS="-L/usr/local/hdf5-${HDF5_VERSION}/lib -L/usr/local/zlib-${ZLIB_VERSION}/lib" \
        CPPFLAGS="-I/usr/local/hdf5-${HDF5_VERSION}/include -I/usr/local/zlib-${ZLIB_VERSION}/include"

RUN cd /tmp/netcdf-c-${NETCDFC_VERSION} && \
    make ${MAKE_OPTS} && \
    make install && \
    cd / && \
    rm -rf /tmp/netcdf-c-${NETCDFC_VERSION}*

# Install NetCDF-Fortran
RUN cd /tmp && \
    wget https://github.com/Unidata/netcdf-fortran/archive/v${NETCDF_VERSION}.tar.gz -O netcdf-fortran-${NETCDF_VERSION}.tar.gz && \
    tar -xzf netcdf-fortran-${NETCDF_VERSION}.tar.gz && \
    cd netcdf-fortran-${NETCDF_VERSION}

# Set up paths for dependencies
ENV PATH="/usr/local/netcdf-${NETCDF_VERSION}/bin:/usr/local/hdf5-${HDF5_VERSION}/bin:$PATH" \
    LIBRARY_PATH="/usr/local/netcdf-${NETCDF_VERSION}/lib:/usr/local/hdf5-${HDF5_VERSION}/lib:/usr/local/zlib-${ZLIB_VERSION}/lib:$LIBRARY_PATH" \
    LD_LIBRARY_PATH="/opt/intel/oneapi/compiler/latest/lib:/opt/intel/oneapi/tcm/latest:$LD_LIBRARY_PATH" \
    CPATH="/usr/local/netcdf-${NETCDF_VERSION}/include:/usr/local/hdf5-${HDF5_VERSION}/include:/usr/local/zlib-${ZLIB_VERSION}/include:$CPATH" \
    PKG_CONFIG_PATH="/usr/local/netcdf-${NETCDF_VERSION}/lib/pkgconfig:/usr/local/hdf5-${HDF5_VERSION}/lib/pkgconfig:/usr/local/zlib-${ZLIB_VERSION}/lib/pkgconfig:$PKG_CONFIG_PA

## icx needs to set LD_LIBRARY_PATH in same run block as configure for netcdf-fortran test compilations ##
## icx also needs -Wno-error=implicit-function-declaration in CLFAGS for netcdf-fortran ##
RUN cd /tmp/netcdf-fortran-${NETCDF_VERSION} && \
    LD_LIBRARY_PATH=/usr/local/netcdf-${NETCDF_VERSION}/lib:/usr/local/hdf5-${HDF5_VERSION}/lib:/usr/local/zlib-${ZLIB_VERSION}/lib:$LD_LIBRARY_PATH \
    ./configure \
        --prefix=/usr/local/netcdf-${NETCDF_VERSION} \
        --enable-shared \
        CC=icx \
        FC=ifx \
        F77=ifx \
        CFLAGS='-O2 -fPIC -Wno-error=implicit-function-declaration' \
        FCFLAGS='-O2 -fPIC' \
        FFLAGS='-O2 -fPIC' \
        CPPFLAGS='-I/usr/local/netcdf-${NETCDF_VERSION}/include -I/usr/local/hdf5-${HDF5_VERSION}/include' \
        LDFLAGS='-L/usr/local/netcdf-${NETCDF_VERSION}/lib -L/usr/local/hdf5-${HDF5_VERSION}/lib -L/usr/local/zlib-${ZLIB_VERSION}/lib' \
        LIBS='-lnetcdf -lhdf5_hl_fortran -lhdf5_fortran -lhdf5_hl -lhdf5 -lz -ldl -lm' && \
        make && make check && make install

RUN cd /tmp/netcdf-fortran-${NETCDF_VERSION} && \
    make ${MAKE_OPTS} && \
    make install && \
    make check && \
    cd / && \
    rm -rf /tmp/netcdf-fortran-${NETCDF_VERSION}* && \
    cd /usr/local && \
    ln -sf netcdf-${NETCDF_VERSION} netcdf

# Stage 2: Build EPS3
FROM netcdf-builder AS eps3-full

ARG EPS3_VERSION
ARG NETCDF_VERSION
ARG HDF5_VERSION

ENV BUILD_DIR="/models.eps3" \
    NETCDF="/usr/local/netcdf-${NETCDF_VERSION}" \
    HDF5="/usr/local/hdf5-${HDF5_VERSION}"

ENV LD_LIBRARY_PATH="$NETCDF/lib:$HDF5/lib:$LD_LIBRARY_PATH"

RUN mkdir -p $BUILD_DIR/src && \
    echo "BUILD_DIR is: $BUILD_DIR/src"

## Copy EPS3 source directory
COPY ./src $BUILD_DIR/src/

## Update Makefile to uncomment ifort/ifx and add chunking
####RUN cd $BUILD_DIR/src && \
####    sed -i \
####    -e 's/^FC = pgf90/###FC = pgf90/' \
####    -e 's/^FLGS = -O2 -Mnoframe/###FLGS = -O2 -Mnoframe/' \
####    -e 's/^###FC = ifort/FC = ifx/' \
####    -e 's/^###FLGS = -O2 -align/FLGS = -O2 -align/' \
####    -e 's/^#CHUNK = -DCHUNK/CHUNK = -DCHUNK/' \
####    -e 's/^CHUNK =$/\#CHUNK =/' \
RUN cd $BUILD_DIR/src && \
    make 2>&1 | tee -a make.ifx.log && \
    cd $BUILD_DIR && \
    mv -v src src.v${EPS3_VERSION}
# Stage 3: Create minimal image and copy from Stage 2 image any libraries needed for EPS3 AND/OR any compiled programs
FROM ubuntu:24.04

ARG ONEAPI_VERSION
ARG NETCDF_VERSION
ARG HDF5_VERSION
ARG EPS3_VERSION

RUN apt-get update

COPY --from=eps3-full /usr/local/netcdf-${NETCDF_VERSION} /usr/local/netcdf-${NETCDF_VERSION}
COPY --from=eps3-full /usr/local/hdf5-${HDF5_VERSION} /usr/local/hdf5-${HDF5_VERSION}
COPY --from=eps3-full /opt/intel/oneapi/compiler/latest/lib /opt/intel/oneapi/compiler/latest/lib
COPY --from=eps3-full /opt/intel/oneapi/compiler/latest/include /opt/intel/oneapi/compiler/latest/include
COPY --from=eps3-full /opt/intel/oneapi/tcm /opt/intel/oneapi/tcm
COPY --from=eps3-full /models.eps3 /models.eps3

ENV NETCDF="/usr/local/netcdf-${NETCDF_VERSION}" \
    HDF5="/usr/local/hdf5-${HDF5_VERSION}" \
    EPS3_VERSION=${EPS3_VERSION}

ENV LD_LIBRARY_PATH="/opt/intel/oneapi/compiler/latest/lib:/opt/intel/oneapi/tcm/latest/lib:$LD_LIBRARY_PATH"
ENV LD_LIBRARY_PATH="$NETCDF/lib:$HDF5/lib:$LD_LIBRARY_PATH"

RUN cd /usr/local && \
    ln -sf netcdf-${NETCDF_VERSION} netcdf && \
    ln -sf hdf5-${HDF5_VERSION} hdf5

# Set generic entrypoint and set CMD to check for EPS3 executable
ENTRYPOINT []
CMD ["/bin/sh","-c","ldd /models.eps3/src.v${EPS3_VERSION}/wrfcamx"]

