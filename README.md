# The News server

## Description
The News REST API server. 

## Personal goals
- Asynchronous logger
- Servant-like type-level routing implementation
- Using TH in favor of Type Driven Development
- Effect tracking system
- As a result - testable code and extensibility

## Setup
Clone current repository:
```
git clone git@github.com:krendelhoff/news-server.git
cd news-server
```

Install postgres and create database:
```
initdb db_name
pg_ctl -D db_name start
createdb db_name
```

Enter psql utility as superuser and create two users: one admin for database changes and second user for general server usage:

```
psql -d database_name

CREATE USER user_name PASSWORD 'user_pass';
GRANT ALL PRIVILEGES ON DATABASE db_name TO user_name;
```

Done! You can leave psql with `\q`.

To run server, you have to provide configuration file either by passing command-line argument or by creating file called "config.yaml" at the root of the project.
```
stack run -- -c <config_path> 
```

To run tests, use can use:
```
stack test
```

## Migrations

All migrations are located at `server/migrations/` folder.

## Logging 
There are four levels of logging verbosity:
- Error - critical errors that lead to program or request handling halting
- Warn - recoverable errors
- Info - notifications about current actions
- Debug - debug information

## CURLS

CURL scripts are located at `scripts/` folder.

## Project structure 

- `server-lib`
  - `src/`
    - `DB.hs` - contains DB related helper functions
    - `Errors.hs` - contains server error definitions
    - `Router.hs` - contains type-level routing implementation
    - `Infrastructure.hs` - namespace with all necessary server-lib functions
    - `Utils.hs` - contains project utility functions.
    - `Types/` 
      - `TH.hs` - contains Template Haskell macros for specialized type creation
      - `Router.hs` - contains router related types
      - `DB.hs` - contains DB related types
      - `Lenses.hs` - contains some lens helpers
      - `Infrastructure` - namespace with all necessary server-lib types
      - `TH/`
        - `Classes.hs` - contains Template Haskell macros for typeclass definition and instances creation

- `server`
  - `src/`
    - `Utils.hs` - contains some utility functions
    - `Migration.hs` - contains applyMigrations function
    - `Logger.hs` - contains asynchronous logger implementation
    - `Effects.hs` - namespace with all effect typeclasses
    - `Effects/` - contains entity-specific effects definitions using typeclasses
      - `<entity>.hs` 
    - `Server.hs` - contains top level server API definition
    - `Server/` - contains API method implementations
      - `Auth.hs` - contains implementation of authentication and "/auth" API
      - `Errors.hs` - contains API method errors
      - `User.hs` - contains API definition for users
      - `User/` - contains API for users implementation 
        - `<entity>.hs`
      - `Admin.hs` - contains API definitions
      - `Admin/` - contains API for admins implementation
        - `<entity>.hs`
    - `Database/` - contains entity-specific database functions
      - `<entity>.hs` 
    - `Types/` 
      - `Auth.hs` - contains types used for authentification
      - `Environment.hs` - contains config, environment and base monad stack definitions
      - `Logger.hs` - contains logger implementation-related types
      - `Utils.hs` - contains types for utility functions
      - `<entity>.hs` - contains entity-specific type definitions
    
## API endpoints

Request examples may be found at folder `scripts/`.

### Route agnostic responses

For auth endpoints:
| Code | Description |
| --- | --- |
| `400` | **Bad request: can't parse request body, present query parameter or capture id** |
| `404` | **Not found** |

For `User` protected endpoints:
| Code | Description |
| --- | --- |
| `400` | **Bad request: can't parse request body, present query parameter or capture id** |
| `401` | **No authorization header, no such token present or token violates format** |
| `403` | **Token expired or not enough rights** |
| `404` | **Not found** |

For `Admin` protected endpoints:
| Code | Description |
| --- | --- |
| `404` | **In case of any authorization errors** |
| `400` | **Bad request: can't parse request body, capture id or present query parameter (if authorized)** |

Every response with error code is accompanied by explanatory messages.

### `/auth`

#### `POST /auth/register`

**Permission**: `None`

Endpoint for user creation.

Request body implements an interface:
```
interface ICreateUserRequest {
  name: string;
  surname: string;
  login: string;
  avatar: string?;      // ID of the picture in database
  password: string;
}
```

Response body implements an interface:
```
interface ICreateUserResponse {
  token: string;
  refreshToken: string;
  expires: timestamptz;
}
```

| Code | Description |
| --- | --- |
| `200` | **Created successfully** |
| `404` | **Picture does not exist** |

[Example](./scripts/auth/register.sh)

#### `POST /auth/login`

**Permission**: `None`

Endpoint for token retrieval.

Request body implements an interface:
```
interface ILoginRequest {
  login: string;
  password: string;
}
```

Response body implements an interface:
```
interface ILoginResponse {
  token: string;
  refreshToken: string;
  expires: timestamptz;
}
```

| Code | Description |
| --- | --- |
| `200` | **Successful login** |
| `404` | **User not found** |

[Example](./scripts/auth/login.sh)

#### `GET /auth/refresh`

**Permission**: `User`

Authorization header must contain refresh token.

Response body implements an interface:
```
interface IRefreshResponse {
  token: string;
  refreshToken: string;
  token: string;
}
```

| Code | Description |
| --- | --- |
| `200` | **Token refreshed successfully** |

[Example](./scripts/auth/refresh.sh)

### `/users`

#### `GET /users` 

**Permission**: `User`

Endpoint for getting current authenticated user.

Response body implements an interface:
```
interface IUserPayloadResponse {
  userId: string;
  name: string;
  surname: timestamptz;
  login: string;
  avatar?: string;
  createdAt: timestamptz;
  privileded: bool;
}
```

| Code | Description |
| --- | --- |
| `200` | **Returns user payload successfully** |

[Example](./scripts/users/get.sh)

#### `DELETE /users/<user_id>` 

**Permission**: `Admin`

Endpoint for deleting users. Returns empty response body with the following codes:

| Code | Description |
| --- | --- |
| `204` | **Removed successfully** |
| `404` | **User not found** |
| `403` | **Not enough rights (attempt to delete an admin)** |

Warning: Deleting user also deletes all entities related to deleted user.

[Example](./scripts/users/delete.sh)

### `/pictures`

#### `POST /pictures`

**Permission**: `User`

Endpoint for posting picture to server. Accepts raw bytes request body with corresponding content-type header.

```
interface IPicturePayloadResponse {
  pictureId: string;
}
```

| Code | Description |
| --- | --- |
| `200` | **Picture uploaded successfully** |

[Example](./scripts/pictures/persist.sh)

### `/categories`

#### `GET /categories/<category_id>` 

**Permission** `User`

Endpoint for getting category information.
Method accepts query parameters:
  - recursive: bool, default true

Response body implements an interface:
```
interface IGetCategoryResponse {
  categoryId: string;
  title: string;
  parent?: string;
}
```

With recursive=true, response body implements an interface:
Response body implements an interface:
```
interface IGetCategoryResponse {
  categoryId: string;
  title: string;
  parent?: IGetCategoryResponse;
}
```

| Code | Description |
| --- | --- |
| `200` | **Returns category payload successfully** |
| `404` | **Category not found** |

[Example](./scripts/categories/get.sh)

#### `POST /categories`

**Permission** `User`

Endpoint for category creation.
Omitted parent field means root category becomes parent.

Request body implements an interface:
```
interface ICreateCategoryRequest {
  title: string;
  parent?: string;
}
```

Response body implements an interface:
```
interface ICreateCategoryResponse {
  categoryId: string;
  title: string;
  parent?: string;
}
```

| Code | Description |
| --- | --- |
| `200` | **Returns category payload successfully** |
| `406` | **Title is not unique error**             |

[Example](./scripts/categories/create.sh)

#### `DELETE /categories/<category_id>`

**Permission** `Admin`

Endpoint for deleting categories. Returns empty response body with the following codes:

| Code | Description |
| --- | --- |
| `200` | **Removed successfully** |
| `403` | **Attempt to remove root category** |
| `404` | **Category not found** |

[Example](./scripts/categories/delete.sh)

#### `PUT /categories/<category_id>`

**Permission** `Admin`

Endpoint for updating categories.

Method accepts query parameters:
  - title: string
  - parent: string (parent category ID)

Response body implements an interface:
```
interface IUpdateCategoryResponse {
  categoryId: string;
  title: string;
  parent?: string;
}
```

| Code | Description |
| --- | --- |
| `200` | **Updated successfully** |
| `404` | **Category not found** |
| `404` | **Rebase destination not found** |
| `406` | **Rebase destination incorrect (forms a cycle)** |
| `406` | **Title is not unique** |

[Example](./scripts/categories/update.sh)

### `/authors`

#### `GET /authors/<author_id>` 

**Permission** `Admin`

Response body implements an interface:
```
interface IGetAuthorResponse {
  userId: string;
  description: string;
}
```

| Code | Description |
| --- | --- |
| `200` | **Returns author payload successfully** |
| `404` | **Author not found** |

[Example](./scripts/authors/get.sh)

#### `POST /authors` 

**Permission** `Admin`

Request body implements an interface:
```
interface IPromoteUserRequest {
  userId: string;
  description: string;
}
```

Response body implements an interface:
```
interface IPromoteUserResponse {
  userId: string;
  description: string;
}
```

| Code | Description |
| --- | --- |
| `200` | **Returns author payload** |
| `404` | **User not found** |
| `406` | **Current user is already an author** |

[Example](./scripts/authors/promote.sh)


#### `PUT /authors/<author_id>` 

**Permission** `Admin`

Request body implements an interface:
```
interface IUpdateAuthorRequest {
  description: string;
}
```

Response body implements an interface:
```
interface IUpdateAuthorResponse {
  userId: string;
  description: string;
}
```

| Code | Description |
| --- | --- |
| `200` | **Returns updated author payload** |
| `404` | **Author not found** |

[Example](./scripts/authors/update.sh)

#### `DELETE /authors/<author_id>` 

**Permission** `Admin`

Endpoint for downgrading author to regular user.

| Code | Description |
| --- | --- |
| `204` | **Downgraded author successfully** |
| `404` | **Author not found** |

[Example](./scripts/authors/downgrade.sh)
