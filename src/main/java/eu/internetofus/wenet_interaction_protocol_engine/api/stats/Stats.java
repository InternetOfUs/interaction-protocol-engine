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
package eu.internetofus.wenet_interaction_protocol_engine.api.stats;

import eu.internetofus.common.model.ErrorMessage;
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
import javax.ws.rs.DefaultValue;
import javax.ws.rs.GET;
import javax.ws.rs.Path;
import javax.ws.rs.Produces;
import javax.ws.rs.QueryParam;
import javax.ws.rs.core.MediaType;

/**
 * Provide stats of the interactions between users.
 *
 * @author UDT-IA, IIIA-CSIC
 */
@Path(Stats.PATH)
@Tag(name = "Stats")
@WebApiServiceGen
public interface Stats {

  /**
   * The path to the service.
   */
  String PATH = "/stats";

  /**
   * The address of this service.
   */
  String ADDRESS = "wenet_interaction_protocol_engine.api.stats";

  /**
   * Called when want to send a incentive to an user that participate on an
   * interaction protocol.
   *
   * @param sourceId      identifier of the source user of the interaction.
   * @param targetId      identifier of the target user of the interaction.
   * @param from          time stamp of the first interaction to start the count,
   *                      or {@code null} to start from the beginning.
   * @param to            time stamp of the last interaction to end the count, or
   *                      {@code null} to end now.
   * @param context       of the request.
   * @param resultHandler to inform of the response.
   */
  @GET
  @Path("/countInteractions")
  @Produces(MediaType.APPLICATION_JSON)
  @Operation(summary = "Count the number of interactions between two users.")
  @ApiResponse(responseCode = "200", description = "If the accepted to be processed", content = @Content(schema = @Schema(implementation = TotalInteractions.class)))
  @ApiResponse(responseCode = "400", description = "Bad request parameters", content = @Content(schema = @Schema(implementation = ErrorMessage.class)))
  void countInteractions(
      @QueryParam(value = "sourceId") @Parameter(description = "The identifier of the source user of the interaction.", example = "1", required = true) String sourceId,
      @QueryParam(value = "targetId") @Parameter(description = "The identifier of the target user of the interaction.", example = "2", required = true) String targetId,
      @QueryParam(value = "from") @Parameter(description = "The difference, measured in seconds, between the first interactions between the users to count and midnight, January 1, 1970 UTC.", example = "1457166440", required = false) Long from,
      @QueryParam(value = "to") @Parameter(description = "The difference, measured in seconds, between the last interactions between the users to count and midnight, January 1, 1970 UTC.", example = "1571664406", required = false) Long to,
      @Parameter(hidden = true, required = false) ServiceRequest context,
      @Parameter(hidden = true, required = false) Handler<AsyncResult<ServiceResponse>> resultHandler);

  /**
   * Called when want to obtains a page with the interactions between two users.
   *
   * @param sourceId      identifier of the source user of the interaction.
   * @param targetId      identifier of the target user of the interaction.
   * @param from          time stamp of the first interaction to start the count,
   *                      or {@code null} to start from the beginning.
   * @param to            time stamp of the last interaction to end the count, or
   *                      {@code null} to end now.
   * @param offset        index of the first interaction to return.
   * @param limit         number maximum of interactions to return.
   * @param context       of the request.
   * @param resultHandler to inform of the response.
   */
  @GET
  @Path("/interactions")
  @Produces(MediaType.APPLICATION_JSON)
  @Operation(summary = "Obtains the interactions between two users in specific time range.")
  @ApiResponse(responseCode = "200", description = "The page with the interactions that match the parameters", content = @Content(schema = @Schema(implementation = InteractionBetweenUsersPage.class)))
  @ApiResponse(responseCode = "400", description = "Bad request parameters", content = @Content(schema = @Schema(implementation = ErrorMessage.class)))
  void getInteractionBetweenUsersPage(
      @QueryParam(value = "sourceId") @Parameter(description = "The identifier of the source user of the interactions to return.", example = "1", required = true) String sourceId,
      @QueryParam(value = "targetId") @Parameter(description = "The identifier of the target user of the interactions to return.", example = "2", required = true) String targetId,
      @QueryParam(value = "from") @Parameter(description = "The difference, measured in seconds, between the first interactions to return and midnight, January 1, 1970 UTC.", example = "1457166440", required = false) Long from,
      @QueryParam(value = "to") @Parameter(description = "The difference, measured in seconds, between the last interactions to return and midnight, January 1, 1970 UTC.", example = "1571664406", required = false) Long to,
      @DefaultValue("0") @QueryParam(value = "offset") @Parameter(description = "The index of the first interaction to return.", example = "4", required = false) int offset,
      @DefaultValue("10") @QueryParam(value = "limit") @Parameter(description = "The number maximum of interactions to return", example = "100", required = false) int limit,
      @Parameter(hidden = true, required = false) ServiceRequest context,
      @Parameter(hidden = true, required = false) Handler<AsyncResult<ServiceResponse>> resultHandler);

}
