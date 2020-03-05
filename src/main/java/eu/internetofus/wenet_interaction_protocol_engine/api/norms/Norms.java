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

package eu.internetofus.wenet_interaction_protocol_engine.api.norms;

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
import io.swagger.v3.oas.annotations.enums.ParameterIn;
import io.swagger.v3.oas.annotations.media.ArraySchema;
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
 * The definition of the web services for manage the norms.
 *
 * @author UDT-IA, IIIA-CSIC
 */
@Path(Norms.PATH)
@Tag(name = "Norms")
@WebApiServiceGen
public interface Norms {

	/**
	 * The path to the version resource.
	 */
	String PATH = "/norms";

	/**
	 * The address of this service.
	 */
	String ADDRESS = "wenet_interaction_protocol_engine.api.norms";

	/**
	 * The sub path to retrieve a published norm.
	 */
	String PUBLISHED_NORM_ID_PATH = "/{publishedNormId}";

	/**
	 * Called when want to publish a norm.
	 *
	 * @param body          the new norm to publish.
	 * @param context       of the request.
	 * @param resultHandler to inform of the response.
	 */
	@POST
	@Consumes(MediaType.APPLICATION_JSON)
	@Produces(MediaType.APPLICATION_JSON)
	@Operation(summary = "Publish a norm", description = "Publish a norm to be accessible by the other users on WeNet")
	@RequestBody(
			description = "The norm to publish",
			required = true,
			content = @Content(schema = @Schema(implementation = PublishedNorm.class)))
	@ApiResponse(
			responseCode = "200",
			description = "The published norm",
			content = @Content(schema = @Schema(implementation = PublishedNorm.class)))
	@ApiResponse(
			responseCode = "400",
			description = "Bad norm to publish",
			content = @Content(schema = @Schema(implementation = ErrorMessage.class)))
	void publishNorm(@Parameter(hidden = true, required = false) JsonObject body,
			@Parameter(hidden = true, required = false) OperationRequest context,
			@Parameter(hidden = true, required = false) Handler<AsyncResult<OperationResponse>> resultHandler);

	/**
	 * Called when want to get some published norms.
	 *
	 * @param context       of the request.
	 * @param resultHandler to inform of the response.
	 */
	@GET
	@Produces(MediaType.APPLICATION_JSON)
	@Operation(
			summary = "Return some published norms",
			description = "Allow to search for a published norms that match some patterns")
	@Parameter(
			in = ParameterIn.QUERY,
			name = "name",
			description = "The name of the published norms to return or a Perl compatible regular expressions (PCRE) that has to match the name of the published norms to return.",
			required = false,
			schema = @Schema(type = "string", example = "(?i)na(?-i)me"))
	@Parameter(
			in = ParameterIn.QUERY,
			name = "description",
			description = "The description of the published norms to return or a Perl compatible regular expressions (PCRE) that has to match the description of the published norms to return.",
			required = false,
			schema = @Schema(type = "string", example = "(?i)descripti(?-i)on"))
	@Parameter(
			in = ParameterIn.QUERY,
			name = "keyword",
			description = "The keyword of the published norms to return or a Perl compatible regular expressions (PCRE) that has to match the keyword of the published norms to return.",
			required = false,
			array = @ArraySchema(schema = @Schema(type = "string", example = "(?i)key(?-i)word")))
	@Parameter(
			in = ParameterIn.QUERY,
			name = "publisherId",
			description = "The identifier of the user that has publish the nrom or a Perl compatible regular expressions (PCRE) that has to match the published identifier of the norms to return.",
			required = false,
			schema = @Schema(type = "string", example = "(?i)a(?-i)vatar"))
	@Parameter(
			in = ParameterIn.QUERY,
			name = "publishFrom",
			description = "The time stamp inclusive that mark the older limit in witch the norm has been published. It is the difference, measured in seconds, between the time when the norm was published and midnight, January 1, 1970 UTC.",
			required = false,
			schema = @Schema(type = "integer", defaultValue = "0", example = "1457166440"))
	@Parameter(
			in = ParameterIn.QUERY,
			name = "publishTo",
			description = "The time stamp inclusive that mark the newest limit in witch the norm has been published. It is the difference, measured in seconds, between the time when the norm was published and midnight, January 1, 1970 UTC.",
			required = false,
			schema = @Schema(type = "integer", defaultValue = "92233720368547757", example = "1571664406"))
	@Parameter(
			in = ParameterIn.QUERY,
			name = "offset",
			description = "Index of the first published norm to return.",
			required = false,
			schema = @Schema(type = "integer", defaultValue = "0", example = "10"))
	@Parameter(
			in = ParameterIn.QUERY,
			name = "limit",
			description = "Number maximum of published norms to return.",
			required = false,
			schema = @Schema(type = "integer", defaultValue = "10", example = "100"))
	@ApiResponse(
			responseCode = "200",
			description = "The published norms that match the patterns",
			content = @Content(schema = @Schema(implementation = PublishedNormsPage.class)))
	@ApiResponse(
			responseCode = "400",
			description = "Bad request. For example if a pattern is not right",
			content = @Content(schema = @Schema(implementation = ErrorMessage.class)))
	void retrievePublishedNormsPage(@Parameter(hidden = true, required = false) OperationRequest context,
			@Parameter(hidden = true, required = false) Handler<AsyncResult<OperationResponse>> resultHandler);

	/**
	 * Called when want to get a published norm.
	 *
	 * @param publishedNormId identifier of the published norm to get.
	 * @param context         of the request.
	 * @param resultHandler   to inform of the response.
	 */
	@GET
	@Path(PUBLISHED_NORM_ID_PATH)
	@Produces(MediaType.APPLICATION_JSON)
	@Operation(
			summary = "Return a published norm associated to the identifier",
			description = "Allow to get a published norm associated to an identifier")
	@ApiResponse(
			responseCode = "200",
			description = "The published norm associated to the identifier",
			content = @Content(schema = @Schema(implementation = PublishedNorm.class)))
	@ApiResponse(
			responseCode = "404",
			description = "Not found published norm",
			content = @Content(schema = @Schema(implementation = ErrorMessage.class)))
	void retrievePublishedNorm(
			@PathParam("publishedNormId") @Parameter(
					description = "The identifier of the published norm to get",
					example = "15837028-645a-4a55-9aaf-ceb846439eba") String publishedNormId,
			@Parameter(hidden = true, required = false) OperationRequest context,
			@Parameter(hidden = true, required = false) Handler<AsyncResult<OperationResponse>> resultHandler);

	/**
	 * Called when want to modify a published norm.
	 *
	 * @param publishedNormId identifier of the published norm to modify.
	 * @param body            the new published norm attributes.
	 * @param context         of the request.
	 * @param resultHandler   to inform of the response.
	 */
	@PUT
	@Path(PUBLISHED_NORM_ID_PATH)
	@Consumes(MediaType.APPLICATION_JSON)
	@Produces(MediaType.APPLICATION_JSON)
	@Operation(summary = "Modify a publishedNorm", description = "Change the attributes of a published norm")
	@RequestBody(
			description = "The new values for the published norm",
			required = true,
			content = @Content(schema = @Schema(implementation = PublishedNorm.class)))
	@ApiResponse(
			responseCode = "200",
			description = "The updated published norm",
			content = @Content(schema = @Schema(implementation = PublishedNorm.class)))
	@ApiResponse(
			responseCode = "400",
			description = "Bad published norm",
			content = @Content(schema = @Schema(implementation = ErrorMessage.class)))
	@ApiResponse(
			responseCode = "404",
			description = "Not found published norm",
			content = @Content(schema = @Schema(implementation = ErrorMessage.class)))
	void updatePublishedNorm(
			@PathParam("publishedNormId") @Parameter(
					description = "The identifier of the published norm to update",
					example = "15837028-645a-4a55-9aaf-ceb846439eba") String publishedNormId,
			@Parameter(hidden = true, required = false) JsonObject body,
			@Parameter(hidden = true, required = false) OperationRequest context,
			@Parameter(hidden = true, required = false) Handler<AsyncResult<OperationResponse>> resultHandler);

	/**
	 * Called when want to delete a published norm.
	 *
	 * @param publishedNormId identifier of the published norm to delete.
	 * @param context         of the request.
	 * @param resultHandler   to inform of the response.
	 */
	@DELETE
	@Path(PUBLISHED_NORM_ID_PATH)
	@Produces(MediaType.APPLICATION_JSON)
	@Operation(
			summary = "Delete the published norm associated to the identifier",
			description = "Allow to delete a published norm associated to an identifier")
	@ApiResponse(responseCode = "204", description = "The published norm was deleted successfully")
	@ApiResponse(
			responseCode = "404",
			description = "Not found published norm",
			content = @Content(schema = @Schema(implementation = ErrorMessage.class)))
	void deletePublishedNorm(
			@PathParam("publishedNormId") @Parameter(
					description = "The identifier of the published norm to delete") String publishedNormId,
			@Parameter(hidden = true, required = false) OperationRequest context,
			@Parameter(hidden = true, required = false) Handler<AsyncResult<OperationResponse>> resultHandler);

}
