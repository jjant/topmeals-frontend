FILTRAR MEALS

MOSTRAR USUARIOS Y PODER BORRARLOS

DESBLOQUEAR USUARIOS

UPLOADEAR PROFILE PHOTO

# Frontend

* Implement at least three roles with different permission levels:

  * Regular user is able to CRUD on their owned records
  * Manager is able CRUD users
  * Admin is able CRUD all records and users.

* Filter by dates from-to, time from-to (e.g. how much calories have I had for lunch each day in the last month if lunch is between 12 and 15h).

- When meals are displayed, they go green if the total for that day is less than expected number of calories per day, otherwise they go red.

- Users have to be able to upload and change their profile picture. If they log in using a social media pull the image from their account they used to log in.

# Backend

* REST API. Make it possible to perform all user actions via the API, including authentication (If a mobile application and you don’t know how to create your own backend you can use Firebase.com or similar services to create the API).

* New users need to verify their account by email. Users should not be able to log in until this verification is complete.

* Additionally, provide an option for the user to log in using at least two social media providers (you can pick from Google, Facebook, Twitter, Github, or similar).

* When a user fails to log in three times in a row, his or her account should be blocked automatically, and only admins and managers should be able to unblock it.

* An admin should be able to invite someone to the application by typing an email address in an input field; the system should then send an invitation message automatically, prompting the user to complete the registration.

* Users have to be able to upload and change their profile picture. If they log in using a social media pull the image from their account they used to log in.

* Write unit and e2e tests.

* If the number of calories is not provided, the API should connect to a Calories API provider (for example https://www.nutritionix.com) and try to get the number of calories for the entered meal.
