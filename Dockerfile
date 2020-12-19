FROM node:15

WORKDIR /usr/src/web

COPY package.json yarn.lock tsconfig.json ./
RUN yarn install --frozen-lockfile

COPY . .

RUN yarn compile
RUN yarn build