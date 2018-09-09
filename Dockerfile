FROM clojure


ENV CLJ_USER=clj_user
ENV CLJ_GROUP=clj_group
ENV HOME_DIR=/home/clj_user
ENV WORK_DIR=/usr/src/app


RUN mkdir -p ${WORK_DIR}

RUN mkdir ${HOME_DIR}


RUN groupadd -g 1000 ${CLJ_GROUP}

RUN useradd -u 1000 -g 1000 -d ${HOME_DIR} ${CLJ_USER}



RUN chown -R ${CLJ_USER}:${CLJ_GROUP} ${HOME_DIR} ${WORK_DIR}


USER ${CLJ_USER}


WORKDIR ${WORK_DIR}

COPY processor/project.clj ${WORK_DIR}/processor/

# RUN cd processor && lein deps

COPY . ${WORK_DIR}

COPY lein/profiles.clj ${HOME_DIR}/.lein/
