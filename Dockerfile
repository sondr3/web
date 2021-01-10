FROM node:15

WORKDIR /usr/src/web

COPY package.json yarn.lock tsconfig.json ./
RUN npm ci

COPY . .

RUN npm run compile
RUN npm run build