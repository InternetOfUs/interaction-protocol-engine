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
import eu.internetofus.wenet_interaction_protocol_engine.api.norms.Norms;
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
	 * The path to the users of a community.
	 */
	String MEMBERS_PATH = "/members";

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
	 * Called when want to get some communities.
	 *
	 * @param context       of the request.
	 * @param resultHandler to inform of the response.
	 */
	@GET
	@Produces(MediaType.APPLICATION_JSON)
	@Operation(
			summary = "Return some active communities",
			description = "Allow to search for a communities that match some patterns")
	@Parameter(
			in = ParameterIn.QUERY,
			name = "name",
			description = "The name of the communities to return or a Perl compatible regular expressions (PCRE) that has to match the name of the communities to return.",
			required = false,
			schema = @Schema(type = "string", example = "(?i)na(?-i)me"))
	@Parameter(
			in = ParameterIn.QUERY,
			name = "description",
			description = "The description of the communities to return or a Perl compatible regular expressions (PCRE) that has to match the description of the communities to return.",
			required = false,
			schema = @Schema(type = "string", example = "(?i)descripti(?-i)on"))
	@Parameter(
			in = ParameterIn.QUERY,
			name = "keyword",
			description = "The keyword of the communities to return or a Perl compatible regular expressions (PCRE) that has to match the keyword of the communities to return.",
			required = false,
			array = @ArraySchema(schema = @Schema(type = "string", example = "(?i)key(?-i)word")))
	@Parameter(
			in = ParameterIn.QUERY,
			name = "avatar",
			description = "The avatar of the communities to return or a Perl compatible regular expressions (PCRE) that has to match the avatar of the communities to return.",
			required = false,
			schema = @Schema(type = "string", example = "(?i)a(?-i)vatar"))
	@Parameter(
			in = ParameterIn.QUERY,
			name = "sinceFrom",
			description = "The time stamp inclusive that mark the older limit in witch the community has been created. It is the difference, measured in seconds, between the time when the profile has to be valid and midnight, January 1, 1970 UTC.",
			required = false,
			schema = @Schema(type = "integer", defaultValue = "0", example = "1457166440"))
	@Parameter(
			in = ParameterIn.QUERY,
			name = "sinceTo",
			description = "The time stamp inclusive that mark the newest limit in witch the community has been created. It is the difference, measured in seconds, between the time when the profile has not more valid and midnight, January 1, 1970 UTC.",
			required = false,
			schema = @Schema(type = "integer", defaultValue = "92233720368547757", example = "1571664406"))
	@Parameter(
			in = ParameterIn.QUERY,
			name = "offset",
			description = "Index of the first community to return.",
			required = false,
			schema = @Schema(type = "integer", defaultValue = "0", example = "10"))
	@Parameter(
			in = ParameterIn.QUERY,
			name = "limit",
			description = "Number maximum of communities to return.",
			required = false,
			schema = @Schema(type = "integer", defaultValue = "10", example = "100"))
	@ApiResponse(
			responseCode = "200",
			description = "The communities that match the patterns",
			content = @Content(schema = @Schema(implementation = CommunitiesPage.class)))
	@ApiResponse(
			responseCode = "400",
			description = "Bad request. For example if a pattern is not right",
			content = @Content(schema = @Schema(implementation = ErrorMessage.class)))
	void retrieveCommunitiesPage(@Parameter(hidden = true, required = false) OperationRequest context,
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

	/**
	 * Called when want to add an user into a community.
	 *
	 * @param communityId   identifier of the community to add the member.
	 * @param body          the new member of the community.
	 * @param context       of the request.
	 * @param resultHandler to inform of the response.
	 */
	@POST
	@Path(COMMUNITY_ID_PATH + MEMBERS_PATH)
	@Consumes(MediaType.APPLICATION_JSON)
	@Produces(MediaType.APPLICATION_JSON)
	@Operation(
			summary = "Add an user into a community",
			description = "Join or add an user to be a member of a community.")
	@RequestBody(
			description = "Member to add into the community",
			required = true,
			content = @Content(schema = @Schema(implementation = Community.class)))
	@ApiResponse(
			responseCode = "200",
			description = "The new member of the community",
			content = @Content(schema = @Schema(implementation = Community.class)))
	@ApiResponse(
			responseCode = "400",
			description = "Bad community member",
			content = @Content(schema = @Schema(implementation = ErrorMessage.class)))
	@ApiResponse(
			responseCode = "404",
			description = "Not found community",
			content = @Content(schema = @Schema(implementation = ErrorMessage.class)))
	void createCommunityMember(
			@PathParam("communityId") @Parameter(
					description = "The identifier of the community to add the member") String communityId,
			@Parameter(hidden = true, required = false) JsonObject body,
			@Parameter(hidden = true, required = false) OperationRequest context,
			@Parameter(hidden = true, required = false) Handler<AsyncResult<OperationResponse>> resultHandler);

	/**
	 * Called when want to remove an user from a community.
	 *
	 * @param communityId   identifier of the community to remove the member.
	 * @param userId        identifier of the user to remove as member of the
	 *                      community.
	 * @param context       of the request.
	 * @param resultHandler to inform of the response.
	 */
	@DELETE
	@Path(COMMUNITY_ID_PATH + MEMBERS_PATH + "/{userId}")
	@Operation(
			summary = "Remove an user from a community",
			description = "You can use this method to leave or remove an user from a community.")
	@ApiResponse(responseCode = "204", description = "The user has removed as member of the community")
	@ApiResponse(
			responseCode = "404",
			description = "Not found community or user",
			content = @Content(schema = @Schema(implementation = ErrorMessage.class)))
	void deleteCommunityMember(
			@PathParam("communityId") @Parameter(
					description = "The identifier of the community to delete the member") String communityId,
			@PathParam("userId") @Parameter(
					description = "The identifier of the user to remove as member of the community") String userId,
			@Parameter(hidden = true, required = false) OperationRequest context,
			@Parameter(hidden = true, required = false) Handler<AsyncResult<OperationResponse>> resultHandler);

	/**
	 * Called when want to get the information of an user in a community.
	 *
	 * @param communityId   identifier of the community to get the member
	 *                      information.
	 * @param userId        identifier of the user to get its membership
	 *                      information.
	 * @param context       of the request.
	 * @param resultHandler to inform of the response.
	 */
	@GET
	@Path(COMMUNITY_ID_PATH + MEMBERS_PATH + "/{userId}")
	@Operation(
			summary = "Get the community member information of an user.",
			description = "You can use this method to obtain the membership information of an user in a community.")
	@ApiResponse(
			responseCode = "200",
			description = "The member of the community",
			content = @Content(schema = @Schema(implementation = Community.class)))
	@ApiResponse(
			responseCode = "404",
			description = "Not found community or user",
			content = @Content(schema = @Schema(implementation = ErrorMessage.class)))
	void retrieveCommunityMember(
			@PathParam("communityId") @Parameter(
					description = "The identifier of the community to get the member information") String communityId,
			@PathParam("userId") @Parameter(
					description = "The identifier of the user to get its membership information in the community") String userId,
			@Parameter(hidden = true, required = false) OperationRequest context,
			@Parameter(hidden = true, required = false) Handler<AsyncResult<OperationResponse>> resultHandler);

	/**
	 * Called when want to get the information of all the user that are in a
	 * community.
	 *
	 * @param communityId   identifier of the community to get the members
	 *                      information.
	 * @param context       of the request.
	 * @param resultHandler to inform of the response.
	 */
	@GET
	@Path(COMMUNITY_ID_PATH + MEMBERS_PATH)
	@Operation(
			summary = "Get the members of a community.",
			description = "You can use this method to obtain the member that are in a community.")
	@Parameter(
			in = ParameterIn.QUERY,
			name = "joinFrom",
			description = "The time stamp inclusive that mark the older limit in witch the community member has joined. It is the difference, measured in seconds, between the time when the member has joined into the community and midnight, January 1, 1970 UTC.",
			required = false,
			schema = @Schema(type = "integer", defaultValue = "0", example = "1457166440"))
	@Parameter(
			in = ParameterIn.QUERY,
			name = "joinTo",
			description = "The time stamp inclusive that mark the newest limit in witch the community member has joined. It is the difference, measured in seconds, between the time when the member has joined into the community and midnight, January 1, 1970 UTC.",
			required = false,
			schema = @Schema(type = "integer", defaultValue = "92233720368547757", example = "1571664406"))
	@Parameter(
			in = ParameterIn.QUERY,
			name = "offset",
			description = "Index of the first community member to return.",
			required = false,
			schema = @Schema(type = "integer", defaultValue = "0", example = "10"))
	@Parameter(
			in = ParameterIn.QUERY,
			name = "limit",
			description = "Number maximum of community members to return.",
			required = false,
			schema = @Schema(type = "integer", defaultValue = "10", example = "100"))
	@ApiResponse(
			responseCode = "200",
			description = "The members of the community that match the pattern",
			content = @Content(schema = @Schema(implementation = CommunityMembersPage.class)))
	@ApiResponse(
			responseCode = "400",
			description = "Bad request. For example if a pattern is not right",
			content = @Content(schema = @Schema(implementation = ErrorMessage.class)))
	@ApiResponse(
			responseCode = "404",
			description = "Not found community",
			content = @Content(schema = @Schema(implementation = ErrorMessage.class)))
	void retrieveCommunityMembersPage(
			@PathParam("communityId") @Parameter(
					description = "The identifier of the community to get the member information") String communityId,
			@Parameter(hidden = true, required = false) OperationRequest context,
			@Parameter(hidden = true, required = false) Handler<AsyncResult<OperationResponse>> resultHandler);

	/**
	 * Called when want to add a norm into a community.
	 *
	 * @param communityId   identifier of the community to add the norm.
	 * @param body          the new norm to add.
	 * @param context       of the request.
	 * @param resultHandler to inform of the response.
	 */
	@POST
	@Path(COMMUNITY_ID_PATH + Norms.PATH)
	@Consumes(MediaType.APPLICATION_JSON)
	@Produces(MediaType.APPLICATION_JSON)
	@Operation(
			summary = "Add an user into a community",
			description = "Since or add an user to be a norm of a community.")
	@RequestBody(
			description = "Norm to add into the community",
			required = true,
			content = @Content(schema = @Schema(implementation = Community.class)))
	@ApiResponse(
			responseCode = "200",
			description = "The new norm of the community",
			content = @Content(schema = @Schema(implementation = Community.class)))
	@ApiResponse(
			responseCode = "400",
			description = "Bad community norm",
			content = @Content(schema = @Schema(implementation = ErrorMessage.class)))
	@ApiResponse(
			responseCode = "404",
			description = "Not found community",
			content = @Content(schema = @Schema(implementation = ErrorMessage.class)))
	void createCommunityNorm(
			@PathParam("communityId") @Parameter(
					description = "The identifier of the community to add the norm") String communityId,
			@Parameter(hidden = true, required = false) JsonObject body,
			@Parameter(hidden = true, required = false) OperationRequest context,
			@Parameter(hidden = true, required = false) Handler<AsyncResult<OperationResponse>> resultHandler);

	/**
	 * Called when want to remove a community from a community.
	 *
	 * @param communityId   identifier of the community to remove the norm.
	 * @param normId        identifier of the norm to delete.
	 * @param context       of the request.
	 * @param resultHandler to inform of the response.
	 */
	@DELETE
	@Path(COMMUNITY_ID_PATH + Norms.PATH + "/{normId}")
	@Operation(
			summary = "Remove a norm from a community",
			description = "You can use this method to remove a norm from a community.")
	@ApiResponse(responseCode = "204", description = "The norm has been removed from the community")
	@ApiResponse(
			responseCode = "404",
			description = "Not found community or norm",
			content = @Content(schema = @Schema(implementation = ErrorMessage.class)))
	void deleteCommunityNorm(
			@PathParam("communityId") @Parameter(
					description = "The identifier of the community to delete the norm") String communityId,
			@PathParam("normId") @Parameter(description = "The identifier of the norm to remove") String normId,
			@Parameter(hidden = true, required = false) OperationRequest context,
			@Parameter(hidden = true, required = false) Handler<AsyncResult<OperationResponse>> resultHandler);

	/**
	 * Called when want to get the information of a norm defined in a community.
	 *
	 * @param communityId   identifier of the community to get the norm.
	 * @param normId        identifier of the norm.
	 * @param context       of the request.
	 * @param resultHandler to inform of the response.
	 */
	@GET
	@Path(COMMUNITY_ID_PATH + Norms.PATH + "/{normId}")
	@Operation(
			summary = "Get the norm defined into a community.",
			description = "You can use this method to obtain a norm that is defined into a community.")
	@ApiResponse(
			responseCode = "200",
			description = "The norm of the community",
			content = @Content(schema = @Schema(implementation = Community.class)))
	@ApiResponse(
			responseCode = "404",
			description = "Not found community or norm",
			content = @Content(schema = @Schema(implementation = ErrorMessage.class)))
	void retrieveCommunityNorm(
			@PathParam("communityId") @Parameter(
					description = "The identifier of the community to get the norm information") String communityId,
			@PathParam("normId") @Parameter(description = "The identifier of the norm to get") String normId,
			@Parameter(hidden = true, required = false) OperationRequest context,
			@Parameter(hidden = true, required = false) Handler<AsyncResult<OperationResponse>> resultHandler);

	/**
	 * Called when want to get the norms associated into a community.
	 *
	 * @param communityId   identifier of the community to get the norms.
	 * @param context       of the request.
	 * @param resultHandler to inform of the response.
	 */
	@GET
	@Path(COMMUNITY_ID_PATH + Norms.PATH)
	@Operation(
			summary = "Get the norms of a community.",
			description = "You can use this method to obtain the norms defined into a community.")
	@Parameter(
			in = ParameterIn.QUERY,
			name = "sinceFrom",
			description = "The time stamp inclusive that mark the older limit in witch the community norm was activated. It is the difference, measured in seconds, between the time when the norm has added into the community and midnight, January 1, 1970 UTC.",
			required = false,
			schema = @Schema(type = "integer", defaultValue = "0", example = "1457166440"))
	@Parameter(
			in = ParameterIn.QUERY,
			name = "sinceTo",
			description = "The time stamp inclusive that mark the newest limit in witch the community norm was activated. It is the difference, measured in seconds, between the time when the norm has added into the community and midnight, January 1, 1970 UTC.",
			required = false,
			schema = @Schema(type = "integer", defaultValue = "92233720368547757", example = "1571664406"))
	@Parameter(
			in = ParameterIn.QUERY,
			name = "offset",
			description = "Index of the first community norm to return.",
			required = false,
			schema = @Schema(type = "integer", defaultValue = "0", example = "10"))
	@Parameter(
			in = ParameterIn.QUERY,
			name = "limit",
			description = "Number maximum of community norms to return.",
			required = false,
			schema = @Schema(type = "integer", defaultValue = "10", example = "100"))
	@ApiResponse(
			responseCode = "200",
			description = "The norms of the community that match the pattern",
			content = @Content(schema = @Schema(implementation = CommunityNormsPage.class)))
	@ApiResponse(
			responseCode = "400",
			description = "Bad request. For example if a pattern is not right",
			content = @Content(schema = @Schema(implementation = ErrorMessage.class)))
	@ApiResponse(
			responseCode = "404",
			description = "Not found community",
			content = @Content(schema = @Schema(implementation = ErrorMessage.class)))
	void retrieveCommunityNormsPage(
			@PathParam("communityId") @Parameter(
					description = "The identifier of the community to get the norms") String communityId,
			@Parameter(hidden = true, required = false) OperationRequest context,
			@Parameter(hidden = true, required = false) Handler<AsyncResult<OperationResponse>> resultHandler);

}
