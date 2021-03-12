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

package eu.internetofus.wenet_interaction_protocol_engine.api.incentives;

import static eu.internetofus.common.components.service.WeNetServiceSimulators.waitUntilCallbacks;
import static eu.internetofus.common.vertx.HttpResponses.assertThatBodyIs;
import static io.reactiverse.junit5.web.TestRequest.testRequest;
import static org.assertj.core.api.Assertions.assertThat;

import eu.internetofus.common.components.ErrorMessage;
import eu.internetofus.common.components.Model;
import eu.internetofus.common.components.incentive_server.Incentive;
import eu.internetofus.common.components.incentive_server.IncentiveTest;
import eu.internetofus.common.components.interaction_protocol_engine.WeNetInteractionProtocolEngine;
import eu.internetofus.common.components.profile_manager.WeNetProfileManager;
import eu.internetofus.common.components.service.App;
import eu.internetofus.common.components.service.Message;
import eu.internetofus.common.components.task_manager.ProtocolNorm;
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
