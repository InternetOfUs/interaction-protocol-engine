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

package eu.internetofus.wenet_interaction_protocol_engine.api.profiles;

import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.responses.ApiResponse;
import io.swagger.v3.oas.annotations.tags.Tag;
import io.vertx.core.AsyncResult;
import io.vertx.core.Handler;
import io.vertx.ext.web.api.service.ServiceRequest;
import io.vertx.ext.web.api.service.ServiceResponse;
import io.vertx.ext.web.api.service.WebApiServiceGen;
import javax.ws.rs.DELETE;
import javax.ws.rs.Path;
import javax.ws.rs.PathParam;
import javax.ws.rs.Produces;
import javax.ws.rs.core.MediaType;

/**
 * Service used to manage the profiles associated to a profile.
 *
 * @author UDT-IA, IIIA-CSIC
 */
@Path(Profiles.PATH)
@Tag(name = "Profiles")
@WebApiServiceGen
public interface Profiles {

  /**
   * The path to the version resource.
   */
  String PATH = "/profiles";

  /**
   * The address of this service.
   */
  String ADDRESS = "wenet_interaction_protocol_engine.api.profiles";

  /**
   * Called when a profile has been deleted.
   *
   * @param profileId     identifier of the profile to delete.
   * @param request       of the operation.
   * @param resultHandler to inform of the response.
   */
  @DELETE
  @Path("/{profileId}")
  @Produces(MediaType.APPLICATION_JSON)
  @Operation(summary = "Called when a profile has been deleted", description = "Allow to delete all information associated to a profile")
  @ApiResponse(responseCode = "204", description = "All the information of the profile was started to be deleted")
  void profileDeleted(
      @PathParam("profileId") @Parameter(description = "The identifier of the profile to delete its information") String profileId,
      @Parameter(hidden = true, required = false) ServiceRequest request,
      @Parameter(hidden = true, required = false) Handler<AsyncResult<ServiceResponse>> resultHandler);

}
