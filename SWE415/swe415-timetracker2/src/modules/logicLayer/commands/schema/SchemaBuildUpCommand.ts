import {Command} from "../Command";

const SchemaBuilder = require("../../../dal/database/schemaBuilder/SchemaBuilder");

export class SchemaBuildUpCommand implements Command{
  execute() {
    SchemaBuilder.buildUp()
  }
}

