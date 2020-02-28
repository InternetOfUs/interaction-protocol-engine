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

package eu.internetofus.wenet_interaction_protocol_engine.api.communities;

import javax.ws.rs.Consumes;
import javax.ws.rs.DELETE;
import javax.ws.rs.GET;
import javax.ws.rs.POST;
import javax.ws.rs.PUT;
import javax.ws.rs.Path;
import javax.ws.rs.PathParam;
import javax.ws.rs.Produces;
import javax.ws.rs.core.MediaType;

import eu.internetofus.wenet_interaction_protocol_engine.api.ErrorMessage;
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
 * The definition of the web services for manage the communities.
 *
 * @author UDT-IA, IIIA-CSIC
 */
@Path(Communities.PATH)
@Tag(name = "Communities")
@WebApiServiceGen
public interface Communities {

	/**
	 * The path to the version resource.
	 */
	String PATH = "/communities";

	/**
	 * The address of this service.
	 */
	String ADDRESS = "wenet_interaction_protocol_engine.api.communities";

	/**
	 * The sub path to retrieve a community.
	 */
	String COMMUNITY_ID_PATH = "/{communityId}";

	/**
	 * Called when want to create a community.
	 *
	 * @param body          the new community to create.
	 * @param context       of the request.
	 * @param resultHandler to inform of the response.
	 */
	@POST
	@Consumes(MediaType.APPLICATION_JSON)
	@Produces(MediaType.APPLICATION_JSON)
	@Operation(summary = "Create a community", description = "Create a new community")
	@RequestBody(
			description = "The new community to create",
			required = true,
			content = @Content(schema = @Schema(implementation = Community.class)))
	@ApiResponse(
			responseCode = "200",
			description = "The created community",
			content = @Content(schema = @Schema(implementation = Community.class)))
	@ApiResponse(
			responseCode = "400",
			description = "Bad community",
			content = @Content(schema = @Schema(implementation = ErrorMessage.class)))
	void createCommunity(@Parameter(hidden = true, required = false) JsonObject body,
			@Parameter(hidden = true, required = false) OperationRequest context,
			@Parameter(hidden = true, required = false) Handler<AsyncResult<OperationResponse>> resultHandler);

	/**
	 * Called when want to get a community.
	 *
	 * @param communityId   identifier of the community to get.
	 * @param context       of the request.
	 * @param resultHandler to inform of the response.
	 */
	@GET
	@Path(COMMUNITY_ID_PATH)
	@Produces(MediaType.APPLICATION_JSON)
	@Operation(
			summary = "Return a community associated to the identifier",
			description = "Allow to get a community associated to an identifier")
	@ApiResponse(
			responseCode = "200",
			description = "The community associated to the identifier",
			content = @Content(schema = @Schema(implementation = Community.class)))
	@ApiResponse(
			responseCode = "404",
			description = "Not found community",
			content = @Content(schema = @Schema(implementation = ErrorMessage.class)))
	void retrieveCommunity(
			@PathParam("communityId") @Parameter(
					description = "The identifier of the community to get",
					example = "15837028-645a-4a55-9aaf-ceb846439eba") String communityId,
			@Parameter(hidden = true, required = false) OperationRequest context,
			@Parameter(hidden = true, required = false) Handler<AsyncResult<OperationResponse>> resultHandler);

	/**
	 * Called when want to modify a community.
	 *
	 * @param communityId   identifier of the community to modify.
	 * @param body          the new community attributes.
	 * @param context       of the request.
	 * @param resultHandler to inform of the response.
	 */
	@PUT
	@Path(COMMUNITY_ID_PATH)
	@Consumes(MediaType.APPLICATION_JSON)
	@Produces(MediaType.APPLICATION_JSON)
	@Operation(summary = "Modify a community", description = "Change the attributes of a community")
	@RequestBody(
			description = "The new values for the community",
			required = true,
			content = @Content(schema = @Schema(implementation = Community.class)))
	@ApiResponse(
			responseCode = "200",
			description = "The updated community",
			content = @Content(schema = @Schema(implementation = Community.class)))
	@ApiResponse(
			responseCode = "400",
			description = "Bad community",
			content = @Content(schema = @Schema(implementation = ErrorMessage.class)))
	@ApiResponse(
			responseCode = "404",
			description = "Not found community",
			content = @Content(schema = @Schema(implementation = ErrorMessage.class)))
	void updateCommunity(
			@PathParam("communityId") @Parameter(
					description = "The identifier of the community to update",
					example = "15837028-645a-4a55-9aaf-ceb846439eba") String communityId,
			@Parameter(hidden = true, required = false) JsonObject body,
			@Parameter(hidden = true, required = false) OperationRequest context,
			@Parameter(hidden = true, required = false) Handler<AsyncResult<OperationResponse>> resultHandler);

	/**
	 * Called when want to delete a community.
	 *
	 * @param communityId   identifier of the community to delete.
	 * @param context       of the request.
	 * @param resultHandler to inform of the response.
	 */
	@DELETE
	@Path(COMMUNITY_ID_PATH)
	@Produces(MediaType.APPLICATION_JSON)
	@Operation(
			summary = "Delete the community associated to the identifier",
			description = "Allow to delete a community associated to an identifier")
	@ApiResponse(responseCode = "204", description = "The community was deleted successfully")
	@ApiResponse(
			responseCode = "404",
			description = "Not found community",
			content = @Content(schema = @Schema(implementation = ErrorMessage.class)))
	void deleteCommunity(
			@PathParam("communityId") @Parameter(
					description = "The identifier of the community to delete") String communityId,
			@Parameter(hidden = true, required = false) OperationRequest context,
			@Parameter(hidden = true, required = false) Handler<AsyncResult<OperationResponse>> resultHandler);

}
