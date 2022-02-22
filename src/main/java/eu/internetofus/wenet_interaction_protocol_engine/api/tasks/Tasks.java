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

package eu.internetofus.wenet_interaction_protocol_engine.api.tasks;

import eu.internetofus.common.model.ErrorMessage;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.media.Content;
import io.swagger.v3.oas.annotations.media.Schema;
import io.swagger.v3.oas.annotations.parameters.RequestBody;
import io.swagger.v3.oas.annotations.responses.ApiResponse;
import io.swagger.v3.oas.annotations.tags.Tag;
import io.vertx.core.AsyncResult;
import io.vertx.core.Handler;
import io.vertx.core.json.JsonObject;
import io.vertx.ext.web.api.service.ServiceRequest;
import io.vertx.ext.web.api.service.ServiceResponse;
import io.vertx.ext.web.api.service.WebApiServiceGen;
import javax.ws.rs.Consumes;
import javax.ws.rs.POST;
import javax.ws.rs.Path;
import javax.ws.rs.Produces;
import javax.ws.rs.core.MediaType;

/**
 * The services to manage the interactions over a task.
 *
 * @author UDT-IA, IIIA-CSIC
 */
@Path(Tasks.PATH)
@Tag(name = "Tasks")
@WebApiServiceGen
public interface Tasks {

  /**
   * The path to the version resource.
   */
  String PATH = "/tasks";

  /**
   * The address of this service.
   */
  String ADDRESS = "wenet_interaction_protocol_engine.api.tasks";

  /**
   * Called when a task is created.
   *
   * @param body          the information of the created task.
   * @param context       of the request.
   * @param resultHandler to inform of the response.
   */
  @Path("/created")
  @POST
  @Consumes(MediaType.APPLICATION_JSON)
  @Produces(MediaType.APPLICATION_JSON)
  @Operation(summary = "Initialize a created task", description = "Start the process to initialize the task execution. Thus, this verify the norms that will be used for example to initialize the task state or decide witch of the WeNet users will participate on the task.")
  @RequestBody(description = "The norm to publish", required = true, content = @Content(schema = @Schema(ref = "https://raw.githubusercontent.com/InternetOfUs/components-documentation/MODELS_2.1.0/sources/wenet-models-openapi.yaml#/components/schemas/Task")))
  @ApiResponse(responseCode = "202", description = "The creation task has accepted to be processed", content = @Content(schema = @Schema(ref = "https://raw.githubusercontent.com/InternetOfUs/components-documentation/MODELS_2.1.0/sources/wenet-models-openapi.yaml#/components/schemas/Task")))
  @ApiResponse(responseCode = "400", description = "Cannot process the created task", content = @Content(schema = @Schema(implementation = ErrorMessage.class)))
  public void taskCreated(@Parameter(hidden = true, required = false) JsonObject body,
      @Parameter(hidden = true, required = false) ServiceRequest context,
      @Parameter(hidden = true, required = false) Handler<AsyncResult<ServiceResponse>> resultHandler);

  /**
   * Called when has to do a transaction over a task.
   *
   * @param body          the transaction to do.
   * @param context       of the request.
   * @param resultHandler to inform of the response.
   */
  @Path("/transactions")
  @POST
  @Consumes(MediaType.APPLICATION_JSON)
  @Produces(MediaType.APPLICATION_JSON)
  @Operation(summary = "Do a transaction over a task", description = "Start the process to do a task transaction. Thus, verify that it fulfill the norms, change the task state and notify the necessary WeNet users.")
  @RequestBody(description = "The norm to publish", required = true, content = @Content(schema = @Schema(ref = "https://raw.githubusercontent.com/InternetOfUs/components-documentation/MODELS_2.1.0/sources/wenet-models-openapi.yaml#/components/schemas/TaskTransaction")))
  @ApiResponse(responseCode = "202", description = "The transaction is accepted to be processed", content = @Content(schema = @Schema(ref = "https://raw.githubusercontent.com/InternetOfUs/components-documentation/MODELS_2.1.0/sources/wenet-models-openapi.yaml#/components/schemas/TaskTransaction")))
  @ApiResponse(responseCode = "400", description = "Cannot process the task transaction", content = @Content(schema = @Schema(implementation = ErrorMessage.class)))
  public void doTransaction(@Parameter(hidden = true, required = false) JsonObject body,
      @Parameter(hidden = true, required = false) ServiceRequest context,
      @Parameter(hidden = true, required = false) Handler<AsyncResult<ServiceResponse>> resultHandler);

}
