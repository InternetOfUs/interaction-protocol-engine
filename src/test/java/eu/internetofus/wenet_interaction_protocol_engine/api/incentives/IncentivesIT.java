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

package eu.internetofus.wenet_interaction_protocol_engine.api.incentives;

import static eu.internetofus.common.components.service.WeNetServiceSimulators.waitUntilCallbacks;
import static eu.internetofus.common.vertx.HttpResponses.assertThatBodyIs;
import static io.reactiverse.junit5.web.TestRequest.testRequest;
import static org.assertj.core.api.Assertions.assertThat;

import eu.internetofus.common.model.ErrorMessage;
import eu.internetofus.common.model.Model;
import eu.internetofus.common.components.models.Incentive;
import eu.internetofus.common.components.models.IncentiveTest;
import eu.internetofus.common.components.interaction_protocol_engine.WeNetInteractionProtocolEngine;
import eu.internetofus.common.components.profile_manager.WeNetProfileManager;
import eu.internetofus.common.components.service.App;
import eu.internetofus.common.components.models.Message;
import eu.internetofus.common.components.models.ProtocolNorm;
import eu.internetofus.wenet_interaction_protocol_engine.WeNetInteractionProtocolEngineIntegrationExtension;
import io.vertx.core.Vertx;
import io.vertx.core.http.HttpMethod;
import io.vertx.ext.web.client.WebClient;
import io.vertx.junit5.VertxTestContext;
import java.util.ArrayList;
import javax.ws.rs.core.Response.Status;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;

/**
 * The integration test over the {@link Incentives}.
 *
 * @see Incentives
 *
 * @author UDT-IA, IIIA-CSIC
 */
@ExtendWith(WeNetInteractionProtocolEngineIntegrationExtension.class)
public class IncentivesIT {

  /**
   * Verify that can not send a bad incentive.
   *
   * @param vertx       event bus to use.
   * @param client      to connect to the server.
   * @param testContext context to test.
   *
   * @see Incentives#sendIncentive(io.vertx.core.json.JsonObject,
   *      io.vertx.ext.web.api.service.ServiceRequest, io.vertx.core.Handler)
   */
  @Test
  public void shouldNotSendBadIncentive(final Vertx vertx, final WebClient client, final VertxTestContext testContext) {

    testRequest(client, HttpMethod.POST, Incentives.PATH).expect(res -> {

      assertThat(res.statusCode()).isEqualTo(Status.BAD_REQUEST.getStatusCode());
      final var error = assertThatBodyIs(ErrorMessage.class, res);
      assertThat(error.code).isNotEmpty().isEqualTo("bad_incentive");
      assertThat(error.message).isNotEmpty().isNotEqualTo(error.code);

    }).sendJson(new IncentiveTest().createModelExample(1).toJsonObject().put("key", "value"), testContext);
  }

  /**
   * Verify that can not send an invalid incentive.
   *
   * @param vertx       event bus to use.
   * @param client      to connect to the server.
   * @param testContext context to test.
   *
   * @see Incentives#sendIncentive(io.vertx.core.json.JsonObject,
   *      io.vertx.ext.web.api.service.ServiceRequest, io.vertx.core.Handler)
   */
  @Test
  public void shouldNotSendInvalidIncentive(final Vertx vertx, final WebClient client,
      final VertxTestContext testContext) {

    final var incentive = new IncentiveTest().createModelExample(1);
    testRequest(client, HttpMethod.POST, Incentives.PATH).expect(res -> {

      assertThat(res.statusCode()).isEqualTo(Status.BAD_REQUEST.getStatusCode());
      final var error = assertThatBodyIs(ErrorMessage.class, res);
      assertThat(error.code).isNotEmpty().isEqualTo("bad_incentive.AppID");
      assertThat(error.message).isNotEmpty().isNotEqualTo(error.code);

    }).sendJson(incentive.toJsonObject(), testContext);
  }

  /**
   * Verify that can send a incentive.
   *
   * @param vertx       event bus to use.
   * @param client      to connect to the server.
   * @param testContext context to test.
   *
   * @see Incentives#sendIncentive(io.vertx.core.json.JsonObject,
   *      io.vertx.ext.web.api.service.ServiceRequest, io.vertx.core.Handler)
   */
  @Test
  public void shouldSendIncentive(final Vertx vertx, final WebClient client, final VertxTestContext testContext) {

    testContext.assertComplete(new IncentiveTest().createModelExample(1, vertx, testContext)).onSuccess(incentive -> {

      testRequest(client, HttpMethod.POST, Incentives.PATH).expect(res -> {

        assertThat(res.statusCode()).isEqualTo(Status.ACCEPTED.getStatusCode());
        final var sent = assertThatBodyIs(Incentive.class, res);
        assertThat(sent).isEqualTo(incentive);

      }).sendJson(incentive.toJsonObject(), testContext);

    });
  }

  /**
   * Verify that can send a incentive using norms.
   *
   * @param vertx       event bus to use.
   * @param client      to connect to the server.
   * @param testContext context to test.
   *
   * @see Incentives#sendIncentive(io.vertx.core.json.JsonObject,
   *      io.vertx.ext.web.api.service.ServiceRequest, io.vertx.core.Handler)
   */
  @Test
  public void shouldSendIncentiveUsingCommunityNorms(final Vertx vertx, final WebClient client,
      final VertxTestContext testContext) {

    testContext.assertComplete(new IncentiveTest().createModelExample(1, vertx, testContext)).onSuccess(incentive -> {

      testContext.assertComplete(App.getOrCreateDefaultCommunityFor(incentive.AppID, vertx)).onSuccess(community -> {

        community.norms = new ArrayList<>();
        final var norm = new ProtocolNorm();
        norm.whenever = "is_received_send_incentive(Incentive)";
        norm.thenceforth = "send_user_message('INCENTIVE',Incentive)";
        community.norms.add(norm);
        testContext.assertComplete(WeNetProfileManager.createProxy(vertx).updateCommunity(community)
            .compose(updatedCommunity -> WeNetInteractionProtocolEngine.createProxy(vertx).sendIncentive(incentive))
            .compose(sentIncentive -> waitUntilCallbacks(incentive.AppID, callbacks -> {

              for (var i = 0; i < callbacks.size(); i++) {

                final var msg = Model.fromJsonObject(callbacks.getJsonObject(i), Message.class);
                if (msg != null && "INCENTIVE".equals(msg.label) && msg.receiverId.equals(incentive.UserId)) {

                  return true;

                }
              }
              return false;

            }, vertx, testContext)

            )

        ).onSuccess(call -> testContext.completeNow());

      });

    });
  }

}
