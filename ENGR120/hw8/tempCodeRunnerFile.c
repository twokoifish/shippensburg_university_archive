  /* Get the second part of the number. */
  token = strtok(NULL, "\n");
  if(token != NULL) {
    /* Store it. */
    sscanf(token, "%d", &(*number).exponent);
  }
}