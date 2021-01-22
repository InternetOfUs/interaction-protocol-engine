/*
 * -----------------------------------------------------------------------------
 *
 * Copyright (c) 1994 - 2021 UDT-IA, IIIA-CSIC
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.
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
package eu.internetofus.wenet_interaction_protocol_engine.api.states;

import eu.internetofus.common.components.ErrorMessage;
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
   * Called to return the state of an user in a community.
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
      @QueryParam(value = "userId") @Parameter(description = "An user identifier to be equals on the state to return. You can use a Perl compatible regular expressions (PCRE) that has to match the user of the state if you write between '/'. For example to get the state for users '1' and '2' you must pass as 'userId' '/^[1|2]$/'.", example = "1", required = false) String userId,
      @Parameter(hidden = true, required = false) ServiceRequest context,
      @Parameter(hidden = true, required = false) Handler<AsyncResult<ServiceResponse>> resultHandler);

  /**
   * Called to return the state of an user in a community.
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
   * Called to return the state of an user in a community.
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
  @ApiResponse(responseCode = "200", description = "The merged community user", content = @Content(schema = @Schema(implementation = State.class)))
  @ApiResponse(responseCode = "400", description = "If the state is not valid", content = @Content(schema = @Schema(implementation = ErrorMessage.class)))
  public void mergeCommunityUserState(
      @PathParam("communityId") @Parameter(description = "The identifier of the community where is the state", example = "1") String communityId,
      @PathParam("userId") @Parameter(description = "The identifier of the user to the related state", example = "2") String userId,
      @Parameter(hidden = true, required = false) JsonObject body,
      @Parameter(hidden = true, required = false) ServiceRequest context,
      @Parameter(hidden = true, required = false) Handler<AsyncResult<ServiceResponse>> resultHandler);

}
