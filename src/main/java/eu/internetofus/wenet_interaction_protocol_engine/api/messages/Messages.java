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

package eu.internetofus.wenet_interaction_protocol_engine.api.messages;

import eu.internetofus.common.model.ErrorMessage;
import eu.internetofus.common.components.interaction_protocol_engine.ProtocolMessage;
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
 * The definition of the web services for manage the messages.
 *
 * @author UDT-IA, IIIA-CSIC
 */
@Path(Messages.PATH)
@Tag(name = "Messages")
@WebApiServiceGen
public interface Messages {

  /**
   * The path to the service.
   */
  String PATH = "/messages";

  /**
   * The address of this service.
   */
  String ADDRESS = "wenet_interaction_protocol_engine.api.messages";

  /**
   * Called when want to send a message in an interaction protocol.
   *
   * @param body          the message to publish on the protocol.
   * @param context       of the request.
   * @param resultHandler to inform of the response.
   */
  @POST
  @Consumes(MediaType.APPLICATION_JSON)
  @Produces(MediaType.APPLICATION_JSON)
  @Operation(summary = "Send a message in an interaction protocol", description = "Publish a message in an interaction protocol that has to be validated by this engine.")
  @RequestBody(description = "The message to publish", required = true, content = @Content(schema = @Schema(implementation = ProtocolMessage.class)))
  @ApiResponse(responseCode = "202", description = "If the message is accepted to be processed", content = @Content(schema = @Schema(implementation = ProtocolMessage.class)))
  @ApiResponse(responseCode = "400", description = "Bad message", content = @Content(schema = @Schema(implementation = ErrorMessage.class)))
  void sendMessage(@Parameter(hidden = true, required = false) JsonObject body,
      @Parameter(hidden = true, required = false) ServiceRequest context,
      @Parameter(hidden = true, required = false) Handler<AsyncResult<ServiceResponse>> resultHandler);

}
