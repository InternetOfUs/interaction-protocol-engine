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

package eu.internetofus.wenet_interaction_protocol_engine.api.events;

import eu.internetofus.common.model.ErrorMessage;
import eu.internetofus.common.components.interaction_protocol_engine.ProtocolEvent;
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
import javax.ws.rs.DELETE;
import javax.ws.rs.POST;
import javax.ws.rs.Path;
import javax.ws.rs.PathParam;
import javax.ws.rs.Produces;
import javax.ws.rs.core.MediaType;

/**
 * The definition of the web services for manage the events.
 *
 * @author UDT-IA, IIIA-CSIC
 */
@Path(Events.PATH)
@Tag(name = "Events")
@WebApiServiceGen
public interface Events {

  /**
   * The path to the service.
   */
  String PATH = "/events";

  /**
   * The address of this service.
   */
  String ADDRESS = "wenet_interaction_protocol_engine.api.events";

  /**
   * Called when want to send a event in an interaction protocol.
   *
   * @param body          the event to publish on the protocol.
   * @param context       of the request.
   * @param resultHandler to inform of the response.
   */
  @POST
  @Consumes(MediaType.APPLICATION_JSON)
  @Produces(MediaType.APPLICATION_JSON)
  @Operation(summary = "Add an event to be fired in an interaction protocol", description = "The event that has to be fired in the norm engine as a message.")
  @RequestBody(description = "The event to publish", required = true, content = @Content(schema = @Schema(implementation = ProtocolEvent.class)))
  @ApiResponse(responseCode = "202", description = "If the event is accepted to be processed", content = @Content(schema = @Schema(implementation = ProtocolEvent.class)))
  @ApiResponse(responseCode = "400", description = "Bad event", content = @Content(schema = @Schema(implementation = ErrorMessage.class)))
  void sendEvent(@Parameter(hidden = true, required = false) JsonObject body,
      @Parameter(hidden = true, required = false) ServiceRequest context,
      @Parameter(hidden = true, required = false) Handler<AsyncResult<ServiceResponse>> resultHandler);

  /**
   * Called when want to delete an event from an interaction protocol.
   *
   * @param id            identifier of the event to be deleted.
   * @param context       of the request.
   * @param resultHandler to inform of the response.
   */
  @DELETE
  @Path("/{id}")
  @Operation(summary = "Delete an event to not be fired on an interaction protocol", description = "Remove an event to not be fired.")
  @ApiResponse(responseCode = "204", description = "If the event is deleted")
  @ApiResponse(responseCode = "404", description = "Not found event", content = @Content(schema = @Schema(implementation = ErrorMessage.class)))
  void deleteEvent(@PathParam("id") @Parameter(description = "The identifier of the event to delete") long id,
      @Parameter(hidden = true, required = false) ServiceRequest context,
      @Parameter(hidden = true, required = false) Handler<AsyncResult<ServiceResponse>> resultHandler);

}
