/*
 * -----------------------------------------------------------------------------
 *
 * Copyright (c) 2019 - 2022 UDT-IA, IIIA-CSIC
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 *
 * -----------------------------------------------------------------------------
 */

package eu.internetofus.wenet_interaction_protocol_engine.api.messages;

import javax.ws.rs.Consumes;
import javax.ws.rs.POST;
import javax.ws.rs.Path;
import javax.ws.rs.Produces;
import javax.ws.rs.core.MediaType;

import eu.internetofus.common.api.models.ErrorMessage;
import eu.internetofus.common.api.models.wenet.InteractionProtocolMessage;
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
import io.vertx.ext.web.api.OperationRequest;
import io.vertx.ext.web.api.OperationResponse;
import io.vertx.ext.web.api.generator.WebApiServiceGen;

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
	@Operation(
			summary = "Send a message in an interaction protocol",
			description = "Publish a message in an interaction protocol that has to be validated by this engine.")
	@RequestBody(
			description = "The message to publish",
			required = true,
			content = @Content(schema = @Schema(implementation = InteractionProtocolMessage.class)))
	@ApiResponse(
			responseCode = "200",
			description = "The published message",
			content = @Content(schema = @Schema(implementation = InteractionProtocolMessage.class)))
	@ApiResponse(
			responseCode = "400",
			description = "Bad message",
			content = @Content(schema = @Schema(implementation = ErrorMessage.class)))
	void sendMessage(@Parameter(hidden = true, required = false) JsonObject body,
			@Parameter(hidden = true, required = false) OperationRequest context,
			@Parameter(hidden = true, required = false) Handler<AsyncResult<OperationResponse>> resultHandler);

}
