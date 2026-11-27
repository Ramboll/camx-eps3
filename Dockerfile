ARG ONEAPI_VERSION="oneapi-hpckit:2025.3.0-0-devel-ubuntu24.04"
ARG EPS3_VERSION="3.4"
ARG NETCDF_VERSION="4.4.4"
ARG NETCDFC_VERSION="4.4.1.1"
ARG HDF5_VERSION="1.14.6"
ARG ZLIB_VERSION="1.3.1"
ARG JASPER_VERSION="2.0.33"
ARG MAKE_OPTS="-j4

# Stage 1: Update OS
FROM docker.io/intel/${ONEAPI_VERSION} AS eps3-base
ARG MAKE_OPTS

# Stage 1: Build libraries for EPS3 -- zlib, hdf5, and netCDF
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

ARG ZLIB_VERSION
ARG HDF5_VERSION
ARG NETCDF_VERSION
ARG NETCDFC_VERSION
ARG MAKE_OPTS

ENV DIR="/usr/local" \
    CC=icx

# Install zlib
RUN cd /tmp && \
    wget https://github.com/madler/zlib/releases/download/v${ZLIB_VERSION}/zlib-${ZLIB_VERSION}.tar.gz && \
    tar -xzf zlib-${ZLIB_VERSION}.tar.gz && \
    cd zlib-${ZLIB_VERSION} && \
    ./configure --prefix=$DIR/grib2 && \
    make ${MAKE_OPTS} && \
    make install && \
    cd / && \
    rm -rf /tmp/zlib-${ZLIB_VERSION}*

# Install Jasper
RUN cd /tmp && \
    wget https://github.com/jasper-software/jasper/releases/download/version-${JASPER_VERSION}/jasper-${JASPER_VERSION}.tar.gz && \
    tar -xzf jasper-${JASPER_VERSION}.tar.gz && \
    cd jasper-${JASPER_VERSION}/build && \
    cmake .. -DCMAKE_INSTALL_PREFIX=$DIR/grib2 -DCMAKE_C_FLAGS="-Wno-incompatible-function-pointer-types -Wno-implicit-function-declaration" && \
    make && \
    make install && \
    cd / && \
    rm -rf /tmp/jasper-${JASPER_VERSION}*

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
        CFLAGS="-O2 -fPIC -I/usr/local/hdf5-${HDF5_VERSION}/include -I/usr/local/grib2/include" \
        LDFLAGS="-L/usr/local/hdf5-${HDF5_VERSION}/lib -L/usr/local/grib2/lib" \
        CPPFLAGS="-I/usr/local/hdf5-${HDF5_VERSION}/include -I/usr/local/grib2/include"

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
    LIBRARY_PATH="/usr/local/netcdf-${NETCDF_VERSION}/lib:/usr/local/hdf5-${HDF5_VERSION}/lib:/usr/local/grib2/lib:$LIBRARY_PATH" \
    LD_LIBRARY_PATH="/opt/intel/oneapi/compiler/latest/lib:/opt/intel/oneapi/tcm/latest:$LD_LIBRARY_PATH" \
    CPATH="/usr/local/netcdf-${NETCDF_VERSION}/include:/usr/local/hdf5-${HDF5_VERSION}/include:/usr/local/grib2/include:$CPATH" \
    PKG_CONFIG_PATH="/usr/local/netcdf-${NETCDF_VERSION}/lib/pkgconfig:/usr/local/hdf5-${HDF5_VERSION}/lib/pkgconfig:/usr/local/grib2/lib/pkgconfig:$PKG_CONFIG_PATH" 

## icx needs to set LD_LIBRARY_PATH in same run block as configure for netcdf-fortran test compilations ##
## icx also needs -Wno-error=implicit-function-declaration in CLFAGS for netcdf-fortran ##
RUN cd /tmp/netcdf-fortran-${NETCDF_VERSION} && \
    LD_LIBRARY_PATH=/usr/local/netcdf-${NETCDF_VERSION}/lib:/usr/local/hdf5-${HDF5_VERSION}/lib:/usr/local/grib2/lib:$LD_LIBRARY_PATH \
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
        LDFLAGS='-L/usr/local/netcdf-${NETCDF_VERSION}/lib -L/usr/local/hdf5-${HDF5_VERSION}/lib -L/usr/local/grib2/lib' \
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
### update me ####
FROM netcdf-builder AS eps3-full

ARG EPS3_VERSION
ARG NETCDF_VERSION
ARG HDF5_VERSION

ENV BUILD_DIR="/models/eps3 \
    SRC_DIR="${BUILD_DIR}/src" \
    NETCDF="/usr/local/netcdf-${NETCDF_VERSION}" \
    HDF5="/usr/local/hdf5-${HDF5_VERSION}"

ENV LD_LIBRARY_PATH="$NETCDF/lib:$HDF5/lib:$JASPERLIB:$LD_LIBRARY_PATH"

## Build EPS3 ##
RUN mkdir -p $SRC_DIR && \
    cd $SRC_DIR && \
    wget "wget https://github.com/Ramboll/camx-eps3/archive/refs/tags/EPS3-v3.40-Oct11-2026.tar.gz" && \
    tar xvzf EPS3-v3.40-Oct11-2026.tar.gz && \
    mv camx-eps3-EPS3-v3.40-Oct11-2026/src src && \
    rm -rvf camx-eps3-EPS3-v3.40-Oct11-2026 && \
    cd src/ && \
    make && \

# Stage 4: Create minimal image and copy from Stage 3 image any libraries needed for CAMx AND/OR any compiled programs
FROM ubuntu:24.04

ARG ONEAPI_VERSION
ARG MPICH_VERSION
ARG NETCDF_VERSION
ARG HDF5_VERSION
ARG EPS_VERSION

LABEL org.opencontainers.image.title="EPS3"
LABEL org.opencontainers.image.version="v${EPS3_VERSION}"
LABEL org.opencontainers.image.source="https://github.com/ramboll/eps3-containers"
LABEL org.opencontainers.image.description="WRF v${WRF_VERSION} and WPS ${WPS_VERSION} with ${ONEAPI_VERSION} compiler and MPICH v${MPICH_VERSION}"

RUN apt-get update && \
    apt-get install -y --no-install-recommends libpng16-16t64 && \
    apt-get clean && rm -rf /var/lib/apt/lists/*

COPY --from=wrf-full /opt/intel/oneapi/compiler/latest/lib/libhwloc.so* /opt/intel/oneapi/compiler/latest/lib/
COPY --from=wrf-full /opt/intel/oneapi/compiler/latest/lib/libifcoremt.so* /opt/intel/oneapi/compiler/latest/lib/
COPY --from=wrf-full /opt/intel/oneapi/compiler/latest/lib/libifport.so* /opt/intel/oneapi/compiler/latest/lib/
COPY --from=wrf-full /opt/intel/oneapi/compiler/latest/lib/libimf.so* /opt/intel/oneapi/compiler/latest/lib/
COPY --from=wrf-full /opt/intel/oneapi/compiler/latest/lib/libintlc.so* /opt/intel/oneapi/compiler/latest/lib/
COPY --from=wrf-full /opt/intel/oneapi/compiler/latest/lib/libirng.so* /opt/intel/oneapi/compiler/latest/lib/
COPY --from=wrf-full /opt/intel/oneapi/compiler/latest/lib/libsvml.so* /opt/intel/oneapi/compiler/latest/lib/
COPY --from=wrf-full /opt/intel/oneapi/tcm /opt/intel/oneapi/tcm

COPY --from=wrf-full /usr/local/mpich-${MPICH_VERSION}/lib/libmpi.so* /usr/local/mpich-${MPICH_VERSION}/lib/
COPY --from=wrf-full /usr/local/mpich-${MPICH_VERSION}/lib/libmpifort.so* /usr/local/mpich-${MPICH_VERSION}/lib/
COPY --from=wrf-full /usr/local/mpich-${MPICH_VERSION}/bin/ /usr/local/mpich-${MPICH_VERSION}/bin/

COPY --from=wrf-full /usr/local/netcdf-${NETCDF_VERSION}/lib/libnetcdf.so* /usr/local/netcdf-${NETCDF_VERSION}/lib/
COPY --from=wrf-full /usr/local/netcdf-${NETCDF_VERSION}/lib/libnetcdff.so* /usr/local/netcdf-${NETCDF_VERSION}/lib/

COPY --from=wrf-full /usr/local/hdf5-${HDF5_VERSION}/lib/libhdf5.so* /usr/local/hdf5-${HDF5_VERSION}/lib/
COPY --from=wrf-full /usr/local/hdf5-${HDF5_VERSION}/lib/libhdf5_fortran.so* /usr/local/hdf5-${HDF5_VERSION}/lib/
COPY --from=wrf-full /usr/local/hdf5-${HDF5_VERSION}/lib/libhdf5_hl.so* /usr/local/hdf5-${HDF5_VERSION}/lib/
COPY --from=wrf-full /usr/local/hdf5-${HDF5_VERSION}/lib/libhdf5hl_fortran.so* /usr/local/hdf5-${HDF5_VERSION}/lib/

COPY --from=wrf-full /usr/local/grib2/lib/libjasper.so* /usr/local/grib2/lib/
COPY --from=wrf-full /usr/local/grib2/lib/libz.so* /usr/local/grib2/lib/

# WRF executables and required runtime files only
COPY --from=wrf-full /models.wrf/WRFV${WRF_VERSION}/run/ /models.wrf/WRFV${WRF_VERSION}/run/
COPY --from=wrf-full /models.wrf/WRFV${WRF_VERSION}/test/ /models.wrf/WRFV${WRF_VERSION}/test/
COPY --from=wrf-full /models.wrf/WRFV${WRF_VERSION}/main/*.exe /models.wrf/WRFV${WRF_VERSION}/main/
COPY --from=wrf-full /models.wrf/WRFV${WRF_VERSION}/phys/noahmp/parameters/ /models.wrf/WRFV${WRF_VERSION}/phys/noahmp/parameters/

# WPS executables and runtime files only
COPY --from=wrf-full /models.wrf/WPS-${WPS_VERSION}/*.exe /models.wrf/WPS-${WPS_VERSION}/
COPY --from=wrf-full /models.wrf/WPS-${WPS_VERSION}/ungrib/Variable_Tables/ /models.wrf/WPS-${WPS_VERSION}/ungrib/Variable_Tables/
COPY --from=wrf-full /models.wrf/WPS-${WPS_VERSION}/geogrid/ /models.wrf/WPS-${WPS_VERSION}/geogrid/
COPY --from=wrf-full /models.wrf/WPS-${WPS_VERSION}/metgrid/ /models.wrf/WPS-${WPS_VERSION}/metgrid/

# Recreate symlinks
RUN ln -sf /models.wrf/WRFV${WRF_VERSION} /models.wrf/WRF && \
    ln -sf /models.wrf/WPS-${WPS_VERSION} /models.wrf/WPS

ENV NETCDF="/usr/local/netcdf-${NETCDF_VERSION}" \
    HDF5="/usr/local/hdf5-${HDF5_VERSION}" \
    JASPERLIB="/usr/local/grib2/lib" \
    MPICH_VERSION=${MPICH_VERSION} \
    WRF_VERSION=${WRF_VERSION}

ENV LD_LIBRARY_PATH="/opt/intel/oneapi/compiler/latest/lib:/opt/intel/oneapi/tcm/latest/lib"
ENV LD_LIBRARY_PATH="$NETCDF/lib:$HDF5/lib:$JASPERLIB:/usr/local/mpich-${MPICH_VERSION}/lib:$LD_LIBRARY_PATH"

RUN cd /usr/local && \
    ln -sf mpich-${MPICH_VERSION} mpich3 && \
    ln -sf netcdf-${NETCDF_VERSION} netcdf && \
    ln -sf hdf5-${HDF5_VERSION} hdf5

# Set CMD to check for WPS ungrib executable libraries
CMD ["/bin/bash","-c","ldd /models.wrf/WPS/ungrib.exe"]
