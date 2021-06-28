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
package eu.internetofus.wenet_interaction_protocol_engine.api.states;

import eu.internetofus.common.model.ErrorMessage;
import eu.internetofus.common.components.interaction_protocol_engine.State;
import eu.internetofus.common.components.interaction_protocol_engine.StatesPage;
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
import javax.ws.rs.GET;
import javax.ws.rs.PATCH;
import javax.ws.rs.Path;
import javax.ws.rs.PathParam;
import javax.ws.rs.Produces;
import javax.ws.rs.QueryParam;
import javax.ws.rs.core.MediaType;

/**
 * The services to interact with the states necessaries for the norm engine.
 *
 * @author UDT-IA, IIIA-CSIC
 */
@Path(States.PATH)
@Tag(name = "States")
@WebApiServiceGen
public interface States {

  /**
   * The path to the version resource.
   */
  String PATH = "/states";

  /**
   * The address of this service.
   */
  String ADDRESS = "wenet_interaction_protocol_engine.api.states";

  /**
   * Called to return the state of a user in a community.
   *
   * @param communityId   identifier of the community associated to the state.
   * @param taskId        identifier of the task associated to the state.
   * @param userId        identifier of the user associated to the state.
   * @param context       of the request.
   * @param resultHandler to inform of the response.
   */
  @GET
  @Consumes(MediaType.APPLICATION_JSON)
  @Operation(summary = "Return the state", description = "Retrieve the state that satisfy the query parameters.")
  @ApiResponse(responseCode = "200", description = "The state associated to the query", content = @Content(schema = @Schema(implementation = StatesPage.class)))
  public void retrieveStates(
      @QueryParam(value = "communityId") @Parameter(description = "A community identifier to be equals on the state to return. You can use a Perl compatible regular expressions (PCRE) that has to match the community identifier of the state if you write between '/'. For example to get the state for communities '1' and '2' you must pass as 'communityId' '/^[1|2]$/'.", example = "1", required = false) String communityId,
      @QueryParam(value = "taskId") @Parameter(description = "An task identifier to be equals on the state to return. You can use a Perl compatible regular expressions (PCRE) that has to match the task of the state if you write between '/'. For example to get the state for tasks '1' and '2' you must pass as 'taskId' '/^[1|2]$/'.", example = "1", required = false) String taskId,
      @QueryParam(value = "userId") @Parameter(description = "a user identifier to be equals on the state to return. You can use a Perl compatible regular expressions (PCRE) that has to match the user of the state if you write between '/'. For example to get the state for users '1' and '2' you must pass as 'userId' '/^[1|2]$/'.", example = "1", required = false) String userId,
      @Parameter(hidden = true, required = false) ServiceRequest context,
      @Parameter(hidden = true, required = false) Handler<AsyncResult<ServiceResponse>> resultHandler);

  /**
   * Called to return the state of a user in a community.
   *
   * @param communityId   identifier of the community associated to the state.
   * @param userId        identifier of the user associated to the state.
   * @param context       of the request.
   * @param resultHandler to inform of the response.
   */
  @Path("/communities/{communityId}/users/{userId}")
  @GET
  @Consumes(MediaType.APPLICATION_JSON)
  @Operation(summary = "Return the state", description = "Retrieve the state that satisfy the query parameters.")
  @ApiResponse(responseCode = "200", description = "The state associated to the community user", content = @Content(schema = @Schema(implementation = State.class)))
  public void retrieveCommunityUserState(
      @PathParam("communityId") @Parameter(description = "The identifier of the community where is the state", example = "1") String communityId,
      @PathParam("userId") @Parameter(description = "The identifier of the user to the related state", example = "2") String userId,
      @Parameter(hidden = true, required = false) ServiceRequest context,
      @Parameter(hidden = true, required = false) Handler<AsyncResult<ServiceResponse>> resultHandler);

  /**
   * Called to merge the state of a user in a community.
   *
   * @param communityId   identifier of the community associated to the state.
   * @param userId        identifier of the user associated to the state.
   * @param body          the new state to merge for the user on the community.
   * @param context       of the request.
   * @param resultHandler to inform of the response.
   */
  @Path("/communities/{communityId}/users/{userId}")
  @PATCH
  @Consumes(MediaType.APPLICATION_JSON)
  @Produces(MediaType.APPLICATION_JSON)
  @Operation(summary = "Merge the state", description = "Merge the current community user with a new one.")
  @RequestBody(description = "The new community user state", required = true, content = @Content(schema = @Schema(implementation = State.class)))
  @ApiResponse(responseCode = "200", description = "The merged community user state", content = @Content(schema = @Schema(implementation = State.class)))
  @ApiResponse(responseCode = "400", description = "If the state is not valid", content = @Content(schema = @Schema(implementation = ErrorMessage.class)))
  public void mergeCommunityUserState(
      @PathParam("communityId") @Parameter(description = "The identifier of the community where is the state", example = "1") String communityId,
      @PathParam("userId") @Parameter(description = "The identifier of the user to the related state", example = "2") String userId,
      @Parameter(hidden = true, required = false) JsonObject body,
      @Parameter(hidden = true, required = false) ServiceRequest context,
      @Parameter(hidden = true, required = false) Handler<AsyncResult<ServiceResponse>> resultHandler);

  /**
   * Called to return the state of a user in a task.
   *
   * @param taskId        identifier of the task associated to the state.
   * @param userId        identifier of the user associated to the state.
   * @param context       of the request.
   * @param resultHandler to inform of the response.
   */
  @Path("/tasks/{taskId}/users/{userId}")
  @GET
  @Consumes(MediaType.APPLICATION_JSON)
  @Operation(summary = "Return the state", description = "Retrieve the state that satisfy the query parameters.")
  @ApiResponse(responseCode = "200", description = "The state associated to the task user", content = @Content(schema = @Schema(implementation = State.class)))
  public void retrieveTaskUserState(
      @PathParam("taskId") @Parameter(description = "The identifier of the task where is the state", example = "1") String taskId,
      @PathParam("userId") @Parameter(description = "The identifier of the user to the related state", example = "2") String userId,
      @Parameter(hidden = true, required = false) ServiceRequest context,
      @Parameter(hidden = true, required = false) Handler<AsyncResult<ServiceResponse>> resultHandler);

  /**
   * Called to merge the state of a user in a task.
   *
   * @param taskId        identifier of the task associated to the state.
   * @param userId        identifier of the user associated to the state.
   * @param body          the new state to merge for the user on the task.
   * @param context       of the request.
   * @param resultHandler to inform of the response.
   */
  @Path("/tasks/{taskId}/users/{userId}")
  @PATCH
  @Consumes(MediaType.APPLICATION_JSON)
  @Produces(MediaType.APPLICATION_JSON)
  @Operation(summary = "Merge the state", description = "Merge the current task user with a new one.")
  @RequestBody(description = "The new task user state", required = true, content = @Content(schema = @Schema(implementation = State.class)))
  @ApiResponse(responseCode = "200", description = "The merged task user state", content = @Content(schema = @Schema(implementation = State.class)))
  @ApiResponse(responseCode = "400", description = "If the state is not valid", content = @Content(schema = @Schema(implementation = ErrorMessage.class)))
  public void mergeTaskUserState(
      @PathParam("taskId") @Parameter(description = "The identifier of the task where is the state", example = "1") String taskId,
      @PathParam("userId") @Parameter(description = "The identifier of the user to the related state", example = "2") String userId,
      @Parameter(hidden = true, required = false) JsonObject body,
      @Parameter(hidden = true, required = false) ServiceRequest context,
      @Parameter(hidden = true, required = false) Handler<AsyncResult<ServiceResponse>> resultHandler);

  /**
   * Called to return the state of a user.
   *
   * @param userId        identifier of the user associated to the state.
   * @param context       of the request.
   * @param resultHandler to inform of the response.
   */
  @Path("/users/{userId}")
  @GET
  @Consumes(MediaType.APPLICATION_JSON)
  @Operation(summary = "Return the state", description = "Retrieve the state that satisfy the query parameters.")
  @ApiResponse(responseCode = "200", description = "The state associated to the user", content = @Content(schema = @Schema(implementation = State.class)))
  public void retrieveUserState(
      @PathParam("userId") @Parameter(description = "The identifier of the user to the related state", example = "2") String userId,
      @Parameter(hidden = true, required = false) ServiceRequest context,
      @Parameter(hidden = true, required = false) Handler<AsyncResult<ServiceResponse>> resultHandler);

  /**
   * Called to merge the state of a user.
   *
   * @param userId        identifier of the user associated to the state.
   * @param body          the new state to merge for the user.
   * @param context       of the request.
   * @param resultHandler to inform of the response.
   */
  @Path("/users/{userId}")
  @PATCH
  @Consumes(MediaType.APPLICATION_JSON)
  @Produces(MediaType.APPLICATION_JSON)
  @Operation(summary = "Merge the state", description = "Merge the current community user with a new one.")
  @RequestBody(description = "The new community user state", required = true, content = @Content(schema = @Schema(implementation = State.class)))
  @ApiResponse(responseCode = "200", description = "The merged user state", content = @Content(schema = @Schema(implementation = State.class)))
  @ApiResponse(responseCode = "400", description = "If the state is not valid", content = @Content(schema = @Schema(implementation = ErrorMessage.class)))
  public void mergeUserState(
      @PathParam("userId") @Parameter(description = "The identifier of the user to the related state", example = "2") String userId,
      @Parameter(hidden = true, required = false) JsonObject body,
      @Parameter(hidden = true, required = false) ServiceRequest context,
      @Parameter(hidden = true, required = false) Handler<AsyncResult<ServiceResponse>> resultHandler);

}
