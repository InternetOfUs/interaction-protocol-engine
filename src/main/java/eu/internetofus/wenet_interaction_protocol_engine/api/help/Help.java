/*
 * -----------------------------------------------------------------------------
 *
 * Copyright 2019 - 2022 UDT-IA, IIIA-CSIC
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *
 * -----------------------------------------------------------------------------
 */

package eu.internetofus.wenet_interaction_protocol_engine.api.help;

import javax.ws.rs.GET;
import javax.ws.rs.Path;
import javax.ws.rs.Produces;
import javax.ws.rs.core.MediaType;

import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.media.Content;
import io.swagger.v3.oas.annotations.media.Schema;
import io.swagger.v3.oas.annotations.responses.ApiResponse;
import io.swagger.v3.oas.annotations.tags.Tag;
import io.vertx.core.AsyncResult;
import io.vertx.core.Handler;
import io.vertx.ext.web.api.service.ServiceRequest;
import io.vertx.ext.web.api.service.ServiceResponse;
import io.vertx.ext.web.api.service.WebApiServiceGen;

/**
 * Resource to provide help about the api.
 *
 * @author UDT-IA, IIIA-CSIC
 */
@Path(Help.PATH)
@Tag(name = "Other")
@WebApiServiceGen
public interface Help {

  /**
   * The path to the help resources.
   */
  String PATH = "/help";

  /**
   * The address of this service.
   */
  String ADDRESS = "wenet_interaction_protocol_engine.api.help";

  /**
   * The path to the information of the API.
   */
  String INFO_PATH = "/info";

  /**
   * The path to the Open API description.
   */
  String OPENAPI_YAML_PATH = "/openapi.yaml";

  /**
   * The handler for the get information about the API.
   *
   * @param context       of the request.
   * @param resultHandler to inform of the response.
   */
  @GET
  @Path(INFO_PATH)
  @Operation(summary = "Get the information about the API", description = "Return the relevant information of the API implementation")
  @ApiResponse(responseCode = "200", description = "The API information", content = @Content(schema = @Schema(implementation = APIInfo.class)))
  @Produces(MediaType.APPLICATION_JSON)
  void getInfo(@Parameter(hidden = true, required = false) ServiceRequest context, @Parameter(hidden = true, required = false) Handler<AsyncResult<ServiceResponse>> resultHandler);

  /**
   * The handler for the get open API description.
   *
   * @param context       of the request.
   * @param resultHandler to inform of the response.
   */
  @GET
  @Path(OPENAPI_YAML_PATH)
  @Operation(summary = "Get the Open API description", description = "Return the Open API description of this API")
  @ApiResponse(responseCode = "200", description = "The API description")
  @Produces("application/yaml")
  void getOpenApi(@Parameter(hidden = true, required = false) ServiceRequest context, @Parameter(hidden = true, required = false) Handler<AsyncResult<ServiceResponse>> resultHandler);

}
